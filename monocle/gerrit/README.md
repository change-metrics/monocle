Test done with https://gerrit-review.googlesource.com w/o authentication

Search a change
---------------

https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#list-changes

Ordered by updated date by default. Possibility to set the number of results and also to skip the first N results.
A flag indicates if more results available.

https://gerrit-review.googlesource.com/changes/?q=status:open


  {
    "id": "modules%2Fgit-refs-filter~stable-2.16~If10b7c7cc95548b8f180299f4c54c630fb8cbde8",
    "project": "modules/git-refs-filter",
    "branch": "stable-2.16",
    "hashtags": [],
    "change_id": "If10b7c7cc95548b8f180299f4c54c630fb8cbde8",
    "subject": "Allow to hide only old closed changes",
    "status": "NEW",
    "created": "2019-12-20 20:47:16.000000000",
    "updated": "2019-12-24 01:11:55.000000000",
    "submit_type": "MERGE_IF_NECESSARY",
    "insertions": 178,
    "deletions": 3,
    "total_comment_count": 0,
    "unresolved_comment_count": 0,
    "has_review_started": true,
    "_number": 249137,
    "owner": {
      "_account_id": 1072905
    },
    "requirements": []
  },

https://gerrit-review.googlesource.com/changes/?q=status:open&o=DETAILED_LABELS

  {
    "id": "plugins%2Fproject-group-structure~stable-2.16~Iadfd319affc2f74d95051e691a3e58c6b47e4c83",
    "project": "plugins/project-group-structure",
    "branch": "stable-2.16",
    "hashtags": [],
    "change_id": "Iadfd319affc2f74d95051e691a3e58c6b47e4c83",
    "subject": "Merge branch \u0027stable-2.15\u0027 into stable-2.16",
    "status": "NEW",
    "created": "2019-12-23 22:46:55.000000000",
    "updated": "2019-12-23 22:47:53.000000000",
    "submit_type": "MERGE_IF_NECESSARY",
    "insertions": 9,
    "deletions": 2,
    "total_comment_count": 0,
    "unresolved_comment_count": 0,
    "has_review_started": true,
    "_number": 249252,
    "owner": {
      "_account_id": 1083195
    },
    "labels": {
      "Verified": {
        "all": [
          {
            "value": 0,
            "permitted_voting_range": {
              "min": -1,
              "max": 1
            },
            "_account_id": 1011123
          },
          {
            "value": 1,
            "date": "2019-12-23 22:47:24.000000000",
            "permitted_voting_range": {
              "min": -1,
              "max": 1
            },
            "_account_id": 1083195
          },
          {
            "value": 0,
            "permitted_voting_range": {
              "min": -1,
              "max": 1
            },
            "_account_id": 1012732
          }
        ],
        "values": {
          "-1": "Fails",
          " 0": "No score",
          "+1": "Verified"
        },
        "default_value": 0
      },
      "Code-Review": {
        "all": [
          {
            "value": 0,
            "permitted_voting_range": {
              "min": -2,
              "max": 2
            },
            "_account_id": 1011123
          },
          {
            "value": 0,
            "permitted_voting_range": {
              "min": -1,
              "max": 1
            },
            "_account_id": 1083195
          },
          {
            "value": 0,
            "permitted_voting_range": {
              "min": -2,
              "max": 2
            },
            "_account_id": 1012732
          }
        ],
        "values": {
          "-2": "Do not submit",
          "-1": "I would prefer that you didn\u0027t submit this",
          " 0": "No score",
          "+1": "Looks good to me, but someone else must approve",
          "+2": "Looks good to me, approved"
        },
        "default_value": 0
      }
    },
    "removable_reviewers": [],
    "reviewers": {
      "REVIEWER": [
        {
          "_account_id": 1011123
        },
        {
          "_account_id": 1012732
        },
        {
          "_account_id": 1083195
        }
      ]
    },
    "pending_reviewers": {},
    "requirements": []
  },


With comments only, maybe we can read Change's event from there:

https://gerrit-review.googlesource.com/changes/?q=status:open&o=MESSAGES

It looks OK, we can extract reviews (label) and comments event. Some data are missing like inline comments info or adding reviewer events(but this is displayed in the UI, so the infos is partial from that API endpoint)

{
    "id": "gerrit~master~I6c6717b3d7bc98ec45c204e9043bab4315aae551",
    "project": "gerrit",
    "branch": "master",
    "hashtags": [],
    "change_id": "I6c6717b3d7bc98ec45c204e9043bab4315aae551",
    "subject": "StandaloneSiteTest: Ignore jgit config",
    "status": "NEW",
    "created": "2019-12-18 01:35:12.000000000",
    "updated": "2019-12-23 18:42:14.000000000",
    "submit_type": "MERGE_IF_NECESSARY",
    "insertions": 5,
    "deletions": 0,
    "total_comment_count": 4,
    "unresolved_comment_count": 1,
    "has_review_started": true,
    "_number": 248756,
    "owner": {
      "_account_id": 1004034
    },
    "messages": [
      {
        "id": "06b8258fd0b20e1b3b8e759eb4be20fdb7624087",
        "tag": "autogenerated:gerrit:newPatchSet",
        "author": {
          "_account_id": 1004034
        },
        "real_author": {
          "_account_id": 1004034
        },
        "date": "2019-12-18 01:35:12.000000000",
        "message": "Uploaded patch set 1.",
        "_revision_number": 1
      },
      {
        "id": "c70e29f1e68be2e1065c82085ad1325b95420edf",
        "tag": "autogenerated:gerrit:newPatchSet",
        "author": {
          "_account_id": 1004034
        },
        "real_author": {
          "_account_id": 1004034
        },
        "date": "2019-12-18 01:39:26.000000000",
        "message": "Uploaded patch set 2: Commit message was updated.",
        "_revision_number": 2
      },
      {
        "id": "71ea92faef62193ea0d0bbe41175aabaf927bdcc",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 02:16:58.000000000",
        "message": "Patch Set 2: Code-Style+1\n\n✅ All files are correctly formatted\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-codestyle/48568/consoleText)",
        "_revision_number": 2
      },
      {
        "id": "143f65ef29fcbba46a75bda67a9498853056723b",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 02:17:08.000000000",
        "message": "Patch Set 2: Verified+1\n\n✅ notedb : SUCCESS\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-verifier-bazel/76850/consoleText)",
        "_revision_number": 2
      },
      {
        "id": "6b1be4a85749f70307b30b77ebd1ee1857bb3d45",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 02:55:46.000000000",
        "message": "Patch Set 1: Code-Style+1\n\n✅ All files are correctly formatted\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-codestyle/48566/consoleText)",
        "_revision_number": 1
      },
      {
        "id": "90ec46c4c8ed9bde098a4564f94dd6712883553f",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 02:55:55.000000000",
        "message": "Patch Set 1: Verified+1\n\n✅ notedb : SUCCESS\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-verifier-bazel/76848/consoleText)",
        "_revision_number": 1
      },
      {
        "id": "3c24e64bbefda42ec86503cbd16030dd32e3c2d1",
        "author": {
          "_account_id": 1011323
        },
        "real_author": {
          "_account_id": 1011323
        },
        "date": "2019-12-18 07:37:58.000000000",
        "message": "Patch Set 2: Code-Review+1\n\n(2 comments)",
        "_revision_number": 2
      },
      {
        "id": "47cfc4f215166619cf85e61efa08a5d4edb404ef",
        "tag": "autogenerated:gerrit:newPatchSet",
        "author": {
          "_account_id": 1004034
        },
        "real_author": {
          "_account_id": 1004034
        },
        "date": "2019-12-18 08:51:14.000000000",
        "message": "Uploaded patch set 3: Commit message was updated.",
        "_revision_number": 3
      },
      {
        "id": "81bd4f0204117a56289d2a9e8aad636ec03e14b0",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 08:55:19.000000000",
        "message": "Patch Set 3: -Code-Style -Verified",
        "_revision_number": 3
      },
      {
        "id": "c628bf0310e0fe4c16bdc0c894bb3c9fd733ecee",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 09:16:25.000000000",
        "message": "Patch Set 3: Code-Style+1\n\n✅ All files are correctly formatted\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-codestyle/48574/consoleText)",
        "_revision_number": 3
      },
      {
        "id": "0426c5ca5996b32e0a5f15024680ee2fc4c36ab8",
        "tag": "autogenerated:jenkins",
        "author": {
          "_account_id": 1022687
        },
        "real_author": {
          "_account_id": 1022687
        },
        "date": "2019-12-18 09:16:34.000000000",
        "message": "Patch Set 3: Verified+1\n\n✅ notedb : SUCCESS\n    (https://gerrit-ci.gerritforge.com/job/Gerrit-verifier-bazel/76856/consoleText)",
        "_revision_number": 3
      },
      {
        "id": "19710f878209888dd81da621e31cc32bff44c09b",
        "author": {
          "_account_id": 1004034
        },
        "real_author": {
          "_account_id": 1004034
        },
        "date": "2019-12-18 09:40:12.000000000",
        "message": "Patch Set 3:\n\n(2 comments)",
        "_revision_number": 3
      },
      {
        "id": "a1fdb7f2a4bc238cbef12296ddeb5d967472e898",
        "author": {
          "_account_id": 1011323
        },
        "real_author": {
          "_account_id": 1011323
        },
        "date": "2019-12-23 18:42:14.000000000",
        "message": "Patch Set 3:\n\nI am still seeing this failure during the tests:\n\n  ERROR org.eclipse.jgit.internal.storage.file.LockFile : Creating lock file \n  /home/davido/.config/jgit/config.lock failed\n  java.nio.file.FileSystemException: /home/davido/.config/jgit/config.lock: Read-only file system\n\nTo reproduce, cherry-pick this Java 10 change, but without switching entirely to Java 10: [1] and run:\n\n  $ bazel test //javatests/com/google/gerrit/acceptance/git:GitmodulesIT\n\nThe full stack trace is here: [2].\n\n[1] https://gerrit-review.googlesource.com/c/gerrit/+/238383\n[2] http://paste.openstack.org/show/787870",
        "_revision_number": 3
      }
    ],
    "requirements": []
  },


This one better:


https://gerrit-review.googlesource.com/changes/?q=status:open&o=MESSAGES&o=REVIEWER_UPDATES&o=DETAILED_ACCOUNTS


{
    "id": "modules%2Fgit-refs-filter~stable-2.16~If10b7c7cc95548b8f180299f4c54c630fb8cbde8",
    "project": "modules/git-refs-filter",
    "branch": "stable-2.16",
    "hashtags": [],
    "change_id": "If10b7c7cc95548b8f180299f4c54c630fb8cbde8",
    "subject": "Allow to hide only old closed changes",
    "status": "NEW",
    "created": "2019-12-20 20:47:16.000000000",
    "updated": "2019-12-24 01:11:55.000000000",
    "submit_type": "MERGE_IF_NECESSARY",
    "insertions": 178,
    "deletions": 3,
    "total_comment_count": 0,
    "unresolved_comment_count": 0,
    "has_review_started": true,
    "_number": 249137,
    "owner": {
      "_account_id": 1072905,
      "name": "Antonio Barone",
      "email": "syntonyze@gmail.com",
      "avatars": [
        {
          "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
          "height": 32
        },
        {
          "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
          "height": 56
        },
        {
          "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
          "height": 100
        },
        {
          "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
          "height": 120
        }
      ]
    },
    "reviewer_updates": [
      {
        "updated": "2019-12-24 01:10:21.000000000",
        "updated_by": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "reviewer": {
          "_account_id": 1083454,
          "name": "Marcin Czech",
          "email": "maczech@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-aSr3F79F4J0/AAAAAAAAAAI/AAAAAAAAAAA/fAIwq8P-axs/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-aSr3F79F4J0/AAAAAAAAAAI/AAAAAAAAAAA/fAIwq8P-axs/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-aSr3F79F4J0/AAAAAAAAAAI/AAAAAAAAAAA/fAIwq8P-axs/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-aSr3F79F4J0/AAAAAAAAAAI/AAAAAAAAAAA/fAIwq8P-axs/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "state": "CC"
      },
      {
        "updated": "2019-12-24 01:10:21.000000000",
        "updated_by": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "reviewer": {
          "_account_id": 1054778,
          "name": "Fabio Ponciroli",
          "email": "ponch78@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-bUYtmfJXE1M/AAAAAAAAAAI/AAAAAAAAAAA/ytsfXn0zFOI/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-bUYtmfJXE1M/AAAAAAAAAAI/AAAAAAAAAAA/ytsfXn0zFOI/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-bUYtmfJXE1M/AAAAAAAAAAI/AAAAAAAAAAA/ytsfXn0zFOI/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-bUYtmfJXE1M/AAAAAAAAAAI/AAAAAAAAAAA/ytsfXn0zFOI/s120-p/photo.jpg",
              "height": 120
            }
          ],
          "status": "🇺🇸"
        },
        "state": "REVIEWER"
      },
      {
        "updated": "2019-12-24 01:10:21.000000000",
        "updated_by": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "reviewer": {
          "_account_id": 1006192,
          "name": "Luca Milanesio",
          "email": "luca.milanesio@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-KmZcmcG3RTc/AAAAAAAAAAI/AAAAAAAAAAA/IKxDtbTIzxE/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-KmZcmcG3RTc/AAAAAAAAAAI/AAAAAAAAAAA/IKxDtbTIzxE/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-KmZcmcG3RTc/AAAAAAAAAAI/AAAAAAAAAAA/IKxDtbTIzxE/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-KmZcmcG3RTc/AAAAAAAAAAI/AAAAAAAAAAA/IKxDtbTIzxE/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "state": "REVIEWER"
      }
    ],
    "messages": [
      {
        "id": "43479e911bd164776e73f369a4c3ebc7e6018ff7",
        "tag": "autogenerated:gerrit:newWipPatchSet",
        "author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "real_author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "date": "2019-12-20 20:47:16.000000000",
        "message": "Uploaded patch set 1.",
        "_revision_number": 1
      },
      {
        "id": "4610ad2c30489e4267766f311130bb4d3ffb3ac4",
        "tag": "autogenerated:gerrit:newWipPatchSet",
        "author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "real_author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "date": "2019-12-24 01:08:49.000000000",
        "message": "Uploaded patch set 2.",
        "_revision_number": 2
      },
      {
        "id": "da09a819a224b513fd0a1be7e2edf7fba94708a5",
        "author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "real_author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "date": "2019-12-24 01:09:41.000000000",
        "message": "Patch Set 2: Verified+1\n\nThis change is ready for review.",
        "_revision_number": 2
      },
      {
        "id": "50dd66f9ff5564ff2e0b83d0db1cbceea7df8e9a",
        "tag": "autogenerated:gerrit:newPatchSet",
        "author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "real_author": {
          "_account_id": 1072905,
          "name": "Antonio Barone",
          "email": "syntonyze@gmail.com",
          "avatars": [
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s32-p/photo.jpg",
              "height": 32
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s56-p/photo.jpg",
              "height": 56
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s100-p/photo.jpg",
              "height": 100
            },
            {
              "url": "https://lh3.googleusercontent.com/-FMG5iz_Oe5I/AAAAAAAAAAI/AAAAAAAAAAA/8jKOs_9cAFg/s120-p/photo.jpg",
              "height": 120
            }
          ]
        },
        "date": "2019-12-24 01:11:55.000000000",
        "message": "Uploaded patch set 3: Commit message was updated.",
        "_revision_number": 3
      }
    ],
    "requirements": []
  },


https://gerrit-review.googlesource.com/changes/?q=after:2019-12-20+repositories:plugins&n=1&o=MESSAGES&o=REVIEWER_UPDATES&o=DETAILED_ACCOUNTS

{
    "id": "config~master~I53aff9433dfd72a85ab514f5d4eff9a1d14b0d55",
    "project": "config",
    "branch": "master",
    "hashtags": [],
    "change_id": "I53aff9433dfd72a85ab514f5d4eff9a1d14b0d55",
    "subject": "cc",
    "status": "MERGED",
    "created": "2019-08-20 11:01:45.000000000",
    "updated": "2019-08-29 08:45:34.000000000",
    "submitted": "2019-08-29 08:45:34.000000000",
    "insertions": 0,
    "deletions": 0,
    "unresolved_comment_count": 0,
    "_number": 45,
    "owner": {
      "_account_id": 1,
      "name": "Software Factory Administrator",
      "email": "admin@sftests-rdo.com",
      "username": "admin",
      "avatars": [
        {
          "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
          "height": 26
        },
        {
          "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
          "height": 32
        },
        {
          "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
          "height": 100
        }
      ]
    },
    "reviewer_updates": [],
    "messages": [
      {
        "id": "1de40bc3_fae118ee",
        "tag": "autogenerated:gerrit:newPatchSet",
        "author": {
          "_account_id": 1,
          "name": "Software Factory Administrator",
          "email": "admin@sftests-rdo.com",
          "username": "admin",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:01:45.000000000",
        "message": "Uploaded patch set 1.",
        "_revision_number": 1
      },
      {
        "id": "3a724529_3ead3a3b",
        "author": {
          "_account_id": 2,
          "name": "Zuul CI",
          "email": "zuul@sftests-rdo.com",
          "username": "zuul",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:02:07.000000000",
        "message": "Patch Set 1:\n\nStarting check jobs.",
        "_revision_number": 1
      },
      {
        "id": "dc574cfd_092bff53",
        "author": {
          "_account_id": 2,
          "name": "Zuul CI",
          "email": "zuul@sftests-rdo.com",
          "username": "zuul",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:04:41.000000000",
        "message": "Patch Set 1: Verified-1\n\nBuild failed.\n\n- config-check https://sftests-rdo.com/logs/45/45/1/check/config-check/f119c3a/ : FAILURE in 2m 15s",
        "_revision_number": 1
      },
      {
        "id": "da9c286f_dbc31e2b",
        "author": {
          "_account_id": 1,
          "name": "Software Factory Administrator",
          "email": "admin@sftests-rdo.com",
          "username": "admin",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:17:38.000000000",
        "message": "Patch Set 1:\n\nrecheck",
        "_revision_number": 1
      },
      {
        "id": "fa919cf1_4d8263b7",
        "author": {
          "_account_id": 2,
          "name": "Zuul CI",
          "email": "zuul@sftests-rdo.com",
          "username": "zuul",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:17:49.000000000",
        "message": "Patch Set 1: -Verified\n\nStarting check jobs.",
        "_revision_number": 1
      },
      {
        "id": "461aa0cb_7850f118",
        "author": {
          "_account_id": 2,
          "name": "Zuul CI",
          "email": "zuul@sftests-rdo.com",
          "username": "zuul",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/b84ad6417342542374e3f1ae45fc4a5c.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-20 11:19:38.000000000",
        "message": "Patch Set 1: Verified+1\n\nBuild succeeded.\n\n- config-check https://sftests-rdo.com/zuul/t/local/build/1c0d8614c90143a3b362336d1c5a450a : SUCCESS in 1m 46s",
        "_revision_number": 1
      },
      {
        "id": "7efa8109_e9e4af45",
        "tag": "autogenerated:gerrit:merged",
        "author": {
          "_account_id": 1,
          "name": "Software Factory Administrator",
          "email": "admin@sftests-rdo.com",
          "username": "admin",
          "avatars": [
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d26",
              "height": 26
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d32",
              "height": 32
            },
            {
              "url": "https://www.gravatar.com/avatar/07074f756b9115b8d755fa32a45643ab.jpg?d\u003didenticon\u0026r\u003dpg\u0026s\u003d100",
              "height": 100
            }
          ]
        },
        "date": "2019-08-29 08:45:34.000000000",
        "message": "Change has been successfully pushed.",
        "_revision_number": 1
      }
    ],
    "_more_changes": true
  }

