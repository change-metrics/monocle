import os
import unittest
import json
import pprint
from deepdiff import DeepDiff

from monocle.github import pullrequest


FIXTURES_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'fixtures')
DATASETS = os.path.join(FIXTURES_DIR, 'datasets')


def load_dataset(name):
    with open(os.path.join(DATASETS, name)) as fd:
        data = json.load(fd)
    return data


def load_pr(name):
    input_pr = load_dataset(name + '_raw.json')
    xtrd_ref = load_dataset(name + '_extracted.json')
    return input_pr, xtrd_ref


class DiffException(Exception):
    def __init__(self, message):
        super().__init__()
        self.message = pprint.pformat(message)

    def __str__(self):
        return '\n\n' + self.message


class TestGithubCrawler(unittest.TestCase):
    def extract_and_compare(self, name):
        input_pr, xtrd_ref = load_pr(name)

        pr_fetcher = pullrequest.PRsFetcher(None, 'https://github.com', None, None)
        xtrd = pr_fetcher.extract_objects([input_pr])

        ddiff = DeepDiff(xtrd_ref, xtrd, ignore_order=True)
        if ddiff:
            raise DiffException(ddiff)

    def test_extract_and_compare_pr1(self):
        """
        Github crawler extracts github.com-morucci-monocle-70
        """
        self.extract_and_compare('github.com-morucci-monocle-70')

    def test_extract_and_compare_pr2(self):
        """
        Github crawler extracts github.com-wazo-platform-wazo-ansible-76
        """
        self.extract_and_compare('github.com-wazo-platform-wazo-ansible-76')
