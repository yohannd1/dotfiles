import std.stdio : stdout, stderr, File;
import std.format : format;
import std.string : strip;
import std.process : environment;
import std.file : timeLastModified, isFile, exists, mkdirRecurse, readText, dirEntries, SpanMode;
import std.path : baseName;
import std.datetime : Clock, SysTime;
import std.array : split, empty;
import std.algorithm : map, filter, each, endsWith, startsWith;
import std.conv : to;
import core.stdc.stdlib : exit;

// The cache format is a TSV file where, for each row, the columns are:
// - Note ID
// - Last modification time (Unix Time)
// - Title
// FIXME: improve format so the titles can have tabs
// TODO: maybe a sqlite database would be faster...

void main() {
    auto home = environment.get("HOME");
    if (home is null) {
        stderr.writeln("error: HOME is not set");
        exit(1);
    }

    auto acrWikiDir = environment.get("ACR_WIKI_DIR");
    if (acrWikiDir is null || acrWikiDir.strip().length == 0) {
        stderr.writeln("error: ACR_WIKI_DIR is not set");
        exit(1);
    }

    auto xdgCacheHome = environment
        .get("XDG_CACHE_HOME")
        .orElse(() => format("%s/.cache", home));

    auto acrCacheDir = environment
        .get("ACR_CACHE_DIR")
        .orElse(() => format("%s/acr", xdgCacheHome));

    auto cacheFilePath = format("%s/list-titles-cache", acrCacheDir);

    bool cacheFileExists = cacheFilePath.exists();
    if (cacheFileExists && !cacheFilePath.isFile()) {
        stderr.writefln("error: cache file (%s) exists but is not a file");
        exit(1);
    }

    auto cacheFileOutdated =
        () => acrWikiDir.timeLastModified() >= cacheFilePath.timeLastModified();


    if (cacheFileExists && !cacheFileOutdated()) {
        // Nothing to do! Just parse the cache file and output its contents.
        cacheFilePath
            .readText().split("\n")
            .filter!(x => !x.empty)
            .map!(x => x.split("\t"))
            .each!(x => stdout.writef("%s %s\n", x[0], x[2]));

    } else if (cacheFileExists) {
        // Open the cache file and check if each entry is outdated.
        // TODO: delete cache of files that don't exist anymore

        alias NoteId = string;
        struct CacheEntry {
            SysTime modTime;
            string title;
        }

        bool shouldWriteCache = false;
        CacheEntry[NoteId] cache;

        // Read the existing data in the cache
        cacheFilePath
            .readText().split("\n")
            .filter!(x => !x.empty)
            .map!(x => x.split("\t"))
            .each!((string[] x) {
                auto time = SysTime.fromUnixTime(x[1].to!long());
                cache[x[0]] = CacheEntry(time, x[2]);
            });

        const string fileExt = ".acr";
        acrWikiDir
            .dirEntries(SpanMode.breadth)
            .map!(x => x.name)
            .filter!(x => x.endsWith(fileExt))
            .each!((string filePath) {
                auto noteId = filePath.baseName()[0..$-fileExt.length];
                auto modTime = filePath.timeLastModified();

                CacheEntry makeNew() {
                    shouldWriteCache = true;
                    auto title = filePath.getNoteTitle();
                    return CacheEntry(modTime, title);
                }

                cache.update(
                    noteId,
                    () => makeNew(),
                    (ref CacheEntry current) {
                        if (current.modTime < modTime)
                            current = makeNew();
                    }
                );

                stdout.writef("%s %s\n", noteId, cache[noteId].title);
            });

        if (shouldWriteCache) {
            auto fd = File(cacheFilePath, "w");
            foreach (noteId, entry; cache) {
                fd.writef("%s\t%s\t%s\n", noteId, entry.modTime.toUnixTime(), entry.title);
            }
        }
    } else {
        // The data file doesn't exist at all.
        // Generate the cache file and output to stdout in parallel.

        auto fd = File(cacheFilePath, "w");

        // TODO: this is partially duplicated from the other case. How to improve it?
        const string fileExt = ".acr";
        acrWikiDir
            .dirEntries(SpanMode.breadth)
            .map!(x => x.name)
            .filter!(x => x.endsWith(fileExt))
            .each!((string filePath) {
                auto noteId = filePath.baseName()[0..$-fileExt.length];
                auto modTime = filePath.timeLastModified();
                auto title = filePath.getNoteTitle();

                fd.writef("%s\t%s\t%s\n", noteId, modTime.toUnixTime(), title);
                stdout.writef("%s %s\n", noteId, title);
            });

        // make sure the cache dir exists
        acrCacheDir.mkdirRecurse();
    }
}

string getNoteTitle(string filePath) {
    const string defaultTitle = "<No Title>";
    const string modernPrefix = "%:title ";

    auto iter = File(filePath).byLine();
    if (iter.empty) return defaultTitle;
    auto firstLine = iter.front().strip().to!string();

    if (firstLine.startsWith(modernPrefix)) {
        return firstLine[modernPrefix.length..$].strip();
    }

    if (firstLine.length >= 2 && firstLine.startsWith("=") && firstLine.endsWith("=")) {
        return firstLine[1..$-1].strip();
    }

    return defaultTitle;
}

U orElse(T, U)(T x, U delegate() func) {
    if (x is null) return func();
    else return x;
}
