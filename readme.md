Skrync ![Java CI](https://github.com/RyanSkraba/skrync/workflows/Java%20CI/badge.svg)
==============================================================================

(This is a project in progress)

My file synchronization tool.

Goals
------------------------------------------------------------------------------

1. Save the state of all files recursively from a root directory:
   size and MD5 or SHA1, date, attributes
   - As a file :heavy_check_mark:
   - Automatically generating a descriptive, dated filename :heavy_check_mark:
   - Showing progress :heavy_check_mark:
   - Interruptable and resumable. 
2. Print some statistics on the files that were discovered.
   - Number of files.
   - Largest files that have duplicates.
   - Largest directories that have duplicates.
3. Compare two states to find the differences.
   - Equals only. :heavy_check_mark:
   - Info.
   - New files. :heavy_check_mark:
   - Deleted files. :heavy_check_mark:
   - Moved and/or renamed.
   - Copied...?
4. Rectify the differences. :x: _(not started)_
   - Progress
   - Copy new files from source to destination.
   - Move files in the destination.
   - Delete files.
   - Log and/or stash all changes that would lose data in the destination.

```bash
# Create with default name (media_username_MYDISK_20210101120000).
# Unlikely that it exists already due to the timestamp.
skrync digest --srcDir /media/username/MYDISK/backup \
    --dstDigest /media/username/MYDISK/.skrync/
skrync digest --srcDir /media/username/MYDISK2/backup \
    --dstDigest /media/username/MYDISK2/.skrync/

# Dry run it (produces a plan)
skrync compare --srcDigest \
    /media/username/MYDISK/backup/.skrync/media_username_MYDISK_20210101120000 \
    --dstDigest \
    /media/username/MYDISK2/backup/.skrync/media_username_MYDISK2_20210101120010

# Do it.
skrync execute --srcDigest \
    /media/username/MYDISK/backup/.skrync/media_username_MYDISK_20210101120000 \
    --dstDigest \
    /media/username/MYDISK2/backup/.skrync/media_username_MYDISK2_20210101120010 \
    --backup /tmp/skrync_changed/
skrync execute --plan /tmp/todo/plan.json --backup /tmp/changed/
```

Running the launcher
------------------------------------------------------------------------------

```bash
mvn package
# Using the fat jar
alias skrync='java -jar '$(pwd)'/target/skrync*-SNAPSHOT.jar'
skrync --help
```
