from istorage import IStorage
from fuzzywuzzy import process
import json
import requests


class StorageJson(IStorage):
    """
    THIS CLASS INTERACTS WITH (INHERITS) THE STORAGE INTERFACE (ABSTRACT CLASS) AND
    IMPLEMENTS THE CRUD METHODS IN THE STORAGE INTERFACE.

    IT GETS THE FILE PATH AS PARAMETER AND READS/WRITES TO JSON FILE
    """

    # Movie API Endpoint
    _REQUEST_URL = f'https://www.omdbapi.com/?apikey=eb462b7d&t='

    def __init__(self, file_path):
        """ CONSTRUCTOR. INITIALIZES THE INSTANCE OF THE OBJECT"""

        # File path
        self._file_path = file_path

        # CREATES THE JSON FILE, IF THE FILE ALREADY EXISTS, DO NOTHING
        try:
            with open(file_path, 'x') as file:
                json.dump({}, file)  # INITIALIZE THE FILE WITH AN EMPTY DICTIONARY
        except FileExistsError:
            pass

        # LOADS THE MOVIES FROM THE JSON FILE
        with open(file_path, 'r') as json_file:
            self._movies_data = json.load(json_file)

    def list_movies(self):
        """ LOADS THE JSON FILE, AND RETURNS THE DATA IN THE FILE AS DICT """

        movies_data = self._movies_data

        # Checks if file is empty
        if len(movies_data) == 0:
            print("empty file!")
            return {}

        movie_list = {movie: [movies_data[movie]['rating'], movies_data[movie]['year']]
                      for movie in movies_data}
        return movie_list

    def add_movie(self, movie_title):
        """
        GETS MOVIE TITLE, YEAR, RATING AND MOVIE POSTER URL,
        FETCH DATA FROM API USING THE MOVIE TITLE,
        LOADS JSON FILE,
        SAVE MOVIE DATA TO THE JSON FILE,
        RETURNS SUCCESS MESSAGE
        """

        # Fetch Movie Data using movie_name
        url = self._REQUEST_URL + movie_title
        movies_data = requests.get(url).json()

        # Load Movies Data
        movies_data_json = self._movies_data
        try:
            movies_data_json[movies_data['Title']] = {'year': movies_data['Year'],
                                                      'rating': movies_data['imdbRating'],
                                                      'image_url': movies_data['Poster'],
                                                      'id': movies_data['imdbID'],
                                                      'note': ' ',
                                                      'country': movies_data['Country']}
        except KeyError:
            return f"Movie '{movie_title}' not found"

        self.save_movie(movies_data_json)
        return f"Movie {movie_title} successfully added"

    def delete_movie(self, movie_title):
        """
        GETS MOVIE TITLE TO DELETE,
        LOAD DATA FROM THE JSON FILE,
        REMOVE THE MOVIE FROM THE DATA AND
        SAVE THE DATA AGAIN TO JSON FILE
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
        LOADS DATA FROM JSON FILE,
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
        """ RETURNS THE MOVIES DATA AS DICT"""
        return self._movies_data

    def save_movie(self, movie_data):
        """ SAVES GIVING MOVIE DATA TO FILE """
        with open(self._file_path, 'w') as json_file:
            json.dump(movie_data, json_file)

    def movie_title_match_suggestion(self, movies_data, movie_title):
        """
        GETS MOVIES DATA (DICT) AND MOVIE TITLE (STR)
        CHECKS IF MOVIE TITLE HAS A CLOSE SIMILARITY WITH ANY KEY IN MOVIES DATA
        IF SIMILARITY SCORE IS GREATER THAN OR EQUAL TO 70,
        THE MATCHED NAME IS SUGGESTED TO THE USER.
        """

        try:
            matched_name, _ = process.extractOne(movie_title,
                                                 movies_data.keys(),
                                                 score_cutoff=70)
        except TypeError:
            return f"Movie {movie_title} doesn't exist!"
        if matched_name:
            return f"Movie {movie_title} doesn't exist! Do you mean '{matched_name}'?"
