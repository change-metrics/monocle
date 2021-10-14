# Monocle.
# Copyright (C) 2019-2021 Monocle authors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import logging
import argparse
import os
import sys
import yaml

from typing import List, Union

from pprint import pprint

from monocle import utils
from monocle.db.db import ELmonocleDB
from monocle.db.db import UnknownQueryException
from monocle.github import pullrequest
from monocle.github import application
from monocle.github import organization
from monocle.github import graphql
from monocle.crawler import Crawler, Runner, GroupCrawler
from monocle import config
from monocle import migrate


def main() -> None:
    parser = argparse.ArgumentParser(prog="monocle")
    parser.add_argument("--loglevel", help="logging level", default="INFO")
    parser.add_argument(
        "--elastic-timeout",
        help="Elasticsearch connection retry timeout",
        default=10,
        type=int,
    )
    parser.add_argument(
        "--elastic-conn", help="Elasticsearch connection info", default="localhost:9200"
    )
    parser.add_argument(
        "--use-ssl",
        help="Use https protocol for communication with Elasticsearch",
        action="store_true",
    )
    parser.add_argument(
        "--insecure",
        help="Skip SSL CA cert validation",
        action="store_false",
    )
    parser.add_argument(
        "--ssl_show_warn",
        help="Skip showing a SSL warning message if it is not signed "
        "by CA authority",
        action="store_false",
    )
    parser.add_argument(
        "--elastic-user",
        help="Username for Elasticsearch authorization",
    )
    parser.add_argument(
        "--elastic-password",
        help="Password for Elasticsearch authorization",
    )
    subparsers = parser.add_subparsers(
        title="Subcommands", description="valid subcommands", dest="command"
    )

    parser_crawler = subparsers.add_parser("crawler", help="Threaded crawlers pool")
    parser_crawler.add_argument(
        "--config", help="Configuration file of the crawlers pool", required=True
    )

    parser_dbmanage = subparsers.add_parser("dbmanage", help="Database manager")
    parser_dbmanage.add_argument("--config", help="Configuration file", required=False)
    parser_dbmanage.add_argument(
        "--delete-repository",
        help="Delete events related to a repository (regexp)",
    )
    parser_dbmanage.add_argument(
        "--delete-workspace",
        help="Delete the workspace",
        action="store_true",
    )
    parser_dbmanage.add_argument(
        "--workspace", help="The workspace name", required=True
    )
    parser_dbmanage.add_argument(
        "--run-migrate",
        help="Run the migration process",
    )

    parser_dbmanage.add_argument(
        "--update-idents",
        help="Update identities",
        action="store_true",
    )

    parser_dbquery = subparsers.add_parser(
        "dbquery", help="Run an existsing query on stored events"
    )
    parser_dbquery.add_argument("--workspace", help="The workspace name", required=True)
    parser_dbquery.add_argument("--name", help="The query name", required=True)
    parser_dbquery.add_argument(
        "--repository", help="Scope to events of repositories (regexp)", required=True
    )
    parser_dbquery.add_argument(
        "--target-branch", help="Scope to events of a target branches (regexp)"
    )
    parser_dbquery.add_argument("--gte", help="Scope to events created after date")
    parser_dbquery.add_argument("--lte", help="Scope to events created before date")
    parser_dbquery.add_argument(
        "--on_cc_gte", help="Scope to events related to changes created after date"
    )
    parser_dbquery.add_argument(
        "--on_cc_lte", help="Scope to events related to changes created before date"
    )
    parser_dbquery.add_argument(
        "--ec-same-date",
        help="Scope to events related to changes created during the "
        "same date bondaries defined by gte/lte arguments",
        action="store_true",
    )
    parser_dbquery.add_argument(
        "--type", help="Scope to events types list (comma separated)"
    )
    parser_dbquery.add_argument(
        "--files", help="Scope to changes containing this file regexp"
    )
    parser_dbquery.add_argument(
        "--state",
        help="Scope to changes with state (comma separated)",
    )
    parser_dbquery.add_argument(
        "--change-ids", help="Scope to change ids (comma separated)"
    )
    parser_dbquery.add_argument("--authors", help="Scope to authors (comma separated)")
    parser_dbquery.add_argument(
        "--approvals", help="Scope to objects with approvals (comma separated)"
    )
    parser_dbquery.add_argument(
        "--exclude-approvals", help="Approvals exclude list (comma separated)"
    )
    parser_dbquery.add_argument(
        "--size", help="Return maximum of size results", default=10
    )
    parser_dbquery.add_argument(
        "--from", help="Starting index of the elements to retrieve", default=0
    )
    parser_dbquery.add_argument(
        "--exclude-authors", help="Authors exclude list (comma separated)"
    )
    parser_dbquery.add_argument(
        "--tests-included",
        help="Scope to changes containing tests",
        action="store_true",
    )
    parser_dbquery.add_argument(
        "--self-merged",
        help="Scope to changes merged by their authors",
        action="store_true",
    )
    parser_dbquery.add_argument(
        "--has-issue-tracker-links",
        help="Scope to changes containing an issue tracker link",
        choices=["generic", "github.com", "altassian.net"],
    )
    parser_dbquery.add_argument(
        "--task-priority",
        help="Scope to changes related to task priorities (comma separated)",
    )
    parser_dbquery.add_argument(
        "--task-severity",
        help="Scope to changes related to task severities (comma separated)",
    )
    parser_dbquery.add_argument(
        "--task-issue-type",
        help="Scope to changes related to task type (comma separated)",
    )

    parser_dbquery.add_argument(
        "--task-score",
        help="Scope to changes related to task score '<op>: <val>'",
    )

    parser_migrate_config = subparsers.add_parser(
        "migrate-config", help="Migrate monocle v0.9 configuration"
    )
    parser_migrate_config.add_argument(
        "--config", help="Configuration file path", required=True
    )

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(thread)d - %(threadName)s - "
        + "%(levelname)s - %(message)s",
    )
    log = logging.getLogger(__name__)

    if not args.command:
        parser.print_usage()
        sys.exit(1)

    if args.command == "migrate-config":
        cfg = open(args.config).read()
        raw = yaml.safe_load(cfg)
        new = config.loadUpgrade(cfg)
        envs = "\n".join(config.get_envs(args.config) + [""])
        if os.path.exists(".secrets"):
            secrets = open(".secrets").read()
        else:
            secrets = ""
        if envs != secrets:
            with open(".secrets", "a") as f:
                f.write(envs)
            print(".secrets: updated")
        if raw != new:
            with open(args.config, "w") as f:
                f.write(yaml.dump(new))
            print("%s: migrated" % args.config)

    if args.command == "crawler":
        realpath = os.path.expanduser(args.config)
        if not os.path.isfile(realpath):
            log.error("Unable to access config: %s" % realpath)
            sys.exit(1)
        configdata = config.load(open(realpath).read())
        config.validate(configdata, config.schema)
        tpool: List[Union[Crawler, GroupCrawler]] = []
        group = {}
        app = None
        if os.getenv("APP_ID") and os.getenv("APP_KEY_PATH"):
            app = application.get_app(os.getenv("APP_ID"), os.getenv("APP_KEY_PATH"))
        for tenant in config.get_workspaces(configdata):
            idents_config = config.get_idents_config(configdata, tenant["index"])
            for crawler_item in tenant.get("crawler", {}).get("github_orgs", []):
                tg = pullrequest.TokenGetter(
                    crawler_item["name"], crawler_item.get("token"), app
                )
                github_c_args = pullrequest.GithubCrawlerArgs(
                    command="github_crawler",
                    org=crawler_item["name"],
                    updated_since=crawler_item["updated_since"],
                    loop_delay=tenant["crawler"]["loop_delay"],
                    repository=crawler_item.get("repository"),
                    base_url=utils.strip_url(crawler_item["base_url"]),
                    token_getter=tg,
                    db=ELmonocleDB(
                        elastic_conn=args.elastic_conn,
                        index=tenant["index"],
                        timeout=args.elastic_timeout,
                        user=args.elastic_user,
                        password=args.elastic_password,
                        use_ssl=args.use_ssl,
                        verify_certs=args.insecure,
                        ssl_show_warn=args.ssl_show_warn,
                    ),
                    idents_config=idents_config,
                )
                gid = crawler_item.get("token")
                if not gid:
                    if app:
                        # No token, if we have a app then get the token from the app
                        gid = app.get_token(org=crawler_item["name"])
                    else:
                        log.info("Skip crawler because no token: %s" % github_c_args)
                        continue
                if gid not in group:
                    group[gid] = GroupCrawler()
                    tpool.append(group[gid])
                if github_c_args.repository:
                    repositories = [github_c_args.repository]
                else:
                    log.info("Discovering repositories in %s ..." % github_c_args.org)
                    # No repository specified for that organization so
                    # try to discover all of them
                    rf = organization.RepositoriesFetcher(
                        graphql.GithubGraphQLQuery(token_getter=tg)
                    )
                    repos = rf.get(github_c_args.org)
                    repositories = [
                        repo["name"] for repo in repos if not repo["isArchived"]
                    ]
                    log.info(
                        "Found %s repositories in %s ..."
                        % (len(repositories), github_c_args.org)
                    )
                for repository in repositories:
                    github_c_args.repository = repository
                    group[gid].add_crawler(Runner(github_c_args))
        log.info("%d configured threads" % len(tpool))
        for cthread in tpool:
            cthread.start()

    if args.command == "dbmanage":

        if args.update_idents and not args.config:
            log.error("Please provide the --config option")
            sys.exit(1)
        if args.update_idents:
            idents_config = config.get_idents_config(
                yaml.safe_load(open(args.config)), args.workspace
            )
        else:
            idents_config = []
        db = ELmonocleDB(
            elastic_conn=args.elastic_conn,
            index=args.workspace,
            idents_config=idents_config,
            user=args.elastic_user,
            password=args.elastic_password,
            use_ssl=args.use_ssl,
            verify_certs=args.insecure,
            ssl_show_warn=args.ssl_show_warn,
        )
        if args.delete_repository:
            db.delete_repository(args.delete_repository)
        if args.delete_workspace:
            db.delete_index()
        if args.update_idents:
            db.update_idents()
        if args.run_migrate:
            try:
                migrate.run_migrate(args.run_migrate, args.elastic_conn, args.workspace)
            except migrate.NotAvailableException:
                log.error(
                    "Error: %s is not a valid migration process" % args.run_migrate
                )

    if args.command == "dbquery":
        db = ELmonocleDB(
            elastic_conn=args.elastic_conn,
            index=args.workspace,
            user=args.elastic_user,
            password=args.elastic_password,
            use_ssl=args.use_ssl,
            verify_certs=args.insecure,
            ssl_show_warn=args.ssl_show_warn,
        )
        params = utils.set_params(args)
        try:
            ret = db.run_named_query(args.name, args.repository.lstrip("^"), params)
        except UnknownQueryException as err:
            log.error("Unable to run query: %s" % err)
            sys.exit(1)
        pprint(ret)


if __name__ == "__main__":
    main()
