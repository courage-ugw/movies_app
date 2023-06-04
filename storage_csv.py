from istorage import IStorage
from fuzzywuzzy import process
import csv
import json
import requests


class StorageCsv(IStorage):
    # Movie API Endpoint
    _REQUEST_URL = f'https://www.omdbapi.com/?apikey=eb462b7d&t='

    def __init__(self, file_path):
        self._file_path = file_path

        # CREATES THE CSV FILE, IF THE FILE ALREADY EXISTS, DO NOTHING
        try:
            with open(file_path, 'x') as csv_file:
                writer = csv.writer(csv_file)
                # writer.writerow({})
        except FileExistsError:
            pass

        # LOADS THE MOVIES FROM THE CSV FILE
        with open(file_path, 'r') as csv_file:
            reader = csv.DictReader(csv_file)
            movies_data_dict = {}
            for row in reader:
                print(row)
                title = row["titles"]
                movie_data = {k: v for k, v in row.items() if k != "titles"}
                movies_data_dict[title] = movie_data
            self._movies_data = movies_data_dict

    def list_movies(self):
        """ LOADS THE CSV FILE, AND RETURNS THE DATA IN THE FILE AS DICT """
        movies_data = self._movies_data
        if len(movies_data) == 0:
            print("empty file!")
            return {}
        movie_list = {movie: [movies_data[movie]['rating'], movies_data[movie]['year']]
                      for
                      movie in movies_data}
        return movie_list

    def add_movie(self, movie_title):
        """
        GETS MOVIE TITLE, YEAR, RATING AND MOVIE POSTER URL,
        FETCH DATA FROM API USING THE MOVIE TITLE,
        LOADS CSV FILE,
        SAVE MOVIE DATA TO THE CSV FILE,
        RETURNS SUCCESS MESSAGE
        """

        # Fetch Movie Data using movie_name
        url = self._REQUEST_URL + movie_title
        movies_data = requests.get(url).json()

        # Load Movies Data from CSV File
        movies_data_csv = self._movies_data
        try:
            movies_data_csv[movies_data['Title']] = {'year': movies_data['Year'],
                                                      'rating': movies_data['imdbRating'],
                                                      'image_url': movies_data['Poster'],
                                                      'id': movies_data['imdbID'],
                                                      'note': ' ',
                                                      'country': movies_data['Country']}
        except KeyError:
            return f"Movie '{movie_title}' not found"

        self.save_movie(movies_data_csv)
        return f"Movie {movie_title} successfully added"

    def delete_movie(self, movie_title):
        """
        GETS MOVIE TITLE TO DELETE,
        LOAD DATA FROM THE CSV FILE,
        REMOVE THE MOVIE FROM THE DATA AND
        SAVE THE DATA AGAIN TO CSV FILE
        """

        movies_data = self._movies_data
        for movie in self._movies_data:
            if movie.lower() == movie_title.lower():
                movies_data.pop(movie)
                self.save_movie(self._movies_data)
                return f"Movie {movie_title.title()} successfully deleted"

        self.save_movie(self._movies_data)
        return self.movie_title_match_suggestion(movies_data, movie_title)

    def update_movie(self, movie_title, movie_note):
        """
        GETS MOVIE TITLE AND NOTES,
        LOADS DATA FROM CSV FILE,
        SEARCH AND FIND THE MOVIE IN THE MOVIE DATA AND
        UPDATE THE DATA WITH THE NOTE
        """

        for movie in self._movies_data:
            if movie.lower() == movie_title.lower():
                self._movies_data[movie].update({'note': movie_note})
                self.save_movie(self._movies_data)
                return f"Movie {movie_title.title()} successfully updated"

        self.save_movie(self._movies_data)
        return self.movie_title_match_suggestion(self._movies_data, movie_title)

    @property
    def movies_data(self):
        return self._movies_data

    def save_movie(self, movie_data):
        """ SAVES GIVING MOVIE DATA TO FILE """

        # Extract the fieldnames from the first item in the dictionary
        fieldnames = ['titles'] + list(movie_data[next(iter(movie_data))].keys())

        with open(self._file_path, "w", newline="") as file:
            writer = csv.DictWriter(file, fieldnames=fieldnames)

            # Write the header row
            writer.writeheader()

            # Write the data rows
            for movie, movie_data in movie_data.items():
                # The movie title is added as a separate column
                # **movie_data means using aksterics to unpack the key:value in the dict
                row_data = {"titles": movie, **movie_data}
                writer.writerow(row_data)

    def movie_title_match_suggestion(self, movies_data, movie_title):
        try:
            matched_name, _ = process.extractOne(movie_title,
                                                 movies_data.keys(),
                                                 score_cutoff=70)
        except TypeError:
            return f"Movie {movie_title} doesn't exist!"
        if matched_name:
            return f"Movie {movie_title} doesn't exist! Do you mean '{matched_name}'?"
