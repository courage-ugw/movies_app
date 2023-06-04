from movie_app import MovieApp
from storage_json import StorageJson
from storage_csv import StorageCsv
import argparse

# Initializing the argparse object
parser = argparse.ArgumentParser(description='Movie App that stores your Favourite '
                                             'Movies')

# Assigning the parse arguments
parser.add_argument('file_name', type=str, help='"file_name": Name of the file. It must '
                                                'include an extension (json or csv) e.g.,'
                                                'john.json, doe.csv')
# Parsing the argument
args = parser.parse_args()


def main():
    """
    MAIN ENTRY OF THE APP
    APP SUPPORTS MULTIPLE FILE STORAGE
    RUNS THE APP BASED ON FILE EXTENSION (JSON OR CSV)
    """

    if args.file_name.endswith('json'):
        storage = StorageJson(args.file_name)
        movie_app = MovieApp(storage)
        movie_app.run()
    elif args.file_name.endswith('csv'):
        storage = StorageCsv(args.file_name)
        movie_app = MovieApp(storage)
        movie_app.run()


if __name__ == "__main__":
    main()
