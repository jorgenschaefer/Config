These are my configuration files for UNIX home directories. If you
clone this, be aware that I break stuff randomly and will happily use
`git push --force`. You’ve been warned.

## Use

Run `initialize.sh`.

Careful, read it first, you might not like everything it does.
Especially the cron jobs.

## Directory Structure

I use an idiosyncratic directory setup for my home which you’ll hate,
and I don’t care.

- `.local/bin/`: Executables, added to `PATH`
- `.cache/tmp/`: Temporary files. Should be able to delete these without data loss
- `Documents/`: User documents, with text or similar formats
- `Downloads/`: Stuff I downloaded. Mostly to keep the clutter out of the main directory
- `Files/`: User files that are not text-based (which would go to `Documents/`)
  - `Archive/`: Stuff I don’t want to delete. Usually archives with the file name format `YYYY-MM-DDTHH:MM-description.tar.xz`
  - `Stuff/`: Place to put files I don’t know where else to put. Might get archived from time to time.
- `Programs/`: Locally-installed programs
- `Projects/`: Stuff I work on. Usually git repositores.
- `Public/`: A publicly-available directory. Similar to what others call `public_html`.

I also use `Books/`, `Music/`, `Pictures/` and `Videos/` when applicable.

## License

GPLv3 or later.
