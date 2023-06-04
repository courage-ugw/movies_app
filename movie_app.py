import logging
import random
import re
import statistics
import sys
import requests
from fuzzywuzzy import process


class MovieApp:
    """
     MovieApp class contains all the logic of the movie app
     (menu, commands etc.).
    """

    # Movie App Menu
    _menu = """ 
Menu:
0. Exit
1. List movies
2. Add movie
3. Delete movie
4. Update movie
5. Stats
6. Random movie
7. Search movie
8. Movies sorted by rating
9. Generate website

Enter choice (0-9): 
    """

    def __init__(self, storage):
        """
        CONSTRUCTOR FOR THE MovieApp Class.
        GETS STORAGE OBJECT AS PARAMETER
        """
        # Storage Object
        self._storage = storage

        # Movies Data from File
        self._movies_data = storage.movies_data

        # HTML File Path
        self._html_file_path = '_static/index_template.html'

        # Title for HTML Page
        self._html_file_title = "Masterschool's Movie App"

        # REGEX pattern for HTML content
        self._search_pattern = '<li>[\s\S]*?<div class="movie">\s*?<a href=".*?" ' \
                               'target="_blank">\s*?<img class="movie-poster" src=".*?" ' \
                               'title=""><\/a>\s*?<div class="movie-title">.*?<\/div>' \
                               '\s*?<div class="movie-year">.*?<\/div>[\s\S]*?<\/li>'

        # Country Flag API
        self._country_flag_url = "https://countriesnow.space/api/v0.1/countries/flag/images"

        # LOADS HTML FILE
        with open(self._html_file_path, 'r') as html_file:
            self._html_content = html_file.read()

    def _command_exit_app(self):
        """ EXITS THE APP """

        print("Bye!")
        sys.exit()

    def _command_list_movies(self):
        """
        CALLS THE list_movies() FUNCTION FROM THE STORAGE CLASS,
        PRINTS INFORMATION TO THE SCREEN FOR THE USER
        """

        # Calls list_movies from the storage class using the storage object
        movies_list = self._storage.list_movies()

        # Checks if file is empty
        if len(movies_list) == 0:
            pass
        else:
            print(f'{len(movies_list)} movies in total: ')
            for movie in movies_list:
                print(f"{movie}: {movies_list[movie][0]}, {movies_list[movie][1]}")

    def _command_add_movie(self):
        """
        PROMPTS USER FOR INPUT COMMAND
        CALLS THE add_movie() FUNCTION FROM THE STORAGE CLASS,
        PRINTS SUCCESS MESSAGE TO THE SCREEN FOR THE USER
        """
        movie_title = input("Enter new movie name: ")
        movie = self._storage.add_movie(movie_title)
        print(movie)

    def _command_update_movie(self):
        """
        PROMPTS USER FOR INPUT COMMAND AND MOVIE NOTES
        CALLS THE update_movie() FUNCTION FROM THE STORAGE CLASS,
        PRINTS SUCCESS MESSAGE TO THE SCREEN FOR THE USER
        """
        movie_title = input("Enter movie name: ")
        movie_note = input("Enter movie note: ").title()
        movie = self._storage.update_movie(movie_title, movie_note)
        print(movie)

    def _command_delete_movie(self):
        """
        PROMPTS USER FOR INPUT COMMAND
        CALLS THE delete_movie() FUNCTION FROM THE STORAGE CLASS,
        PRINTS SUCCESS MESSAGE TO THE SCREEN FOR THE USER
        """
        movie_title = input("Enter movie name to delete: ")
        movie = self._storage.delete_movie(movie_title)
        print(movie)

    def _command_movie_stats(self):
        """
        GETS MOVIE TITLES AND RATINGS,
        CALCUALTES THE MEAN AND MEDIAN USING THE RATINGS
        PRINTS THE INFORMATION TO THE SCREEN FOR THE USER
        """
        # Gets only movie titles and ratings
        titles_and_ratings = {title: float(self._movies_data[title]['rating'])
                              for title in self._movies_data}

        # Calculate average and median of ratings
        average_rating = statistics.mean(titles_and_ratings.values())
        median_rating = statistics.median(titles_and_ratings.values())

        # Sort the data using the ratings
        movies_sorted = sorted(titles_and_ratings.items(), key=lambda item: item[1],
                               reverse=True)

        # Unpack best movie and worst movie
        best_movie, best_rating = movies_sorted[0]
        worst_movie, worst_rating = movies_sorted[-1]

        print(f"""
Average rating: {average_rating :.2f}
Median rating: {median_rating}
Best Movie: {best_movie}, {best_rating}
Worst Movie: {worst_movie}, {worst_rating}
        """)

    def _command_random_movie(self):
        """
        LOAD MOVIES DATA FROM FILE,
        GETS ONLY MOVIE TITLES AND RATINGS,
        USE THE RANDOM FUNCTION TO GET A RANDOM MOVIE TITLE AND RATING,
        PRINT THE GENERATED RANDOM MOVIE AND RATING TO THE SCREEN
        """

        # Gets only movie titles and ratings
        titles_and_ratings = {title: float(self._movies_data[title]['rating'])
                              for title in self._movies_data}

        # Generates random movie and its rating
        title, rating = random.choice(list(titles_and_ratings.items()))
        print(f"Your movie for tonight: {title}, it's rated {rating}")

    def _command_search_movies(self):
        """
        GETS MOVIE TITLE OF OR PART OF MOVIE TITLE TO SEARCH FOR,
        CHECK FOR SIMILARITY OF MOVIE TITLE IN MOVIES DATA DICTIONARY USING FUZZYWUZZY,
        IF MOVIE IS FOUND, PRINT MOVIE TITLE AND RATING, ELSE PRINT MOVIE NOT FOUND
        """

        # Suppresses warning from fuzzywuzzy library if input doesn't contain any common
        # characters of a natural language
        logging.getLogger().setLevel(logging.ERROR)

        # Gets input from user
        movie_title = input('Enter part of movie name: ')

        # Gets only movie titles and ratings
        titles_and_ratings = {title: float(self._movies_data[title]['rating'])
                              for title in self._movies_data}

        # Checks for similarity of movie_title in movies dict using the fuzzywuzzy library
        search_result = process.extractOne(movie_title, titles_and_ratings.keys(),
                                           score_cutoff=70)

        if search_result:
            for movie, rating in titles_and_ratings.items():
                if search_result[0].lower() == movie.lower():
                    print(f"{movie}, {rating}")
        else:
            print(f'The string "{movie_title}" does not match any movie name!')

    def _sort_movies_by_rating(self):
        """
        GETS ONLY THE MOVIE TITLES AND RATINGS,
        SORTS DATA BASED ON RATINGS,
        PRINTS THE SORTED DATA TO THE SCREEN
        """

        # Gets only movie titles and ratings
        titles_and_ratings = {title: float(self._movies_data[title]['rating'])
                              for title in self._movies_data}

        # Sort movies by ratings
        movies_sorted = sorted(titles_and_ratings.items(), key=lambda item: item[1],
                               reverse=True)
        for movie in movies_sorted:
            print(f'{movie[0]}: {movie[1]}')

    def _generate_website(self):
        """
        LOADS CONTENT FROM HTML FILE, LOADS MOVIES DATA FROM FILE AND
        SERIALIZES THE DATA FOR HTML. REPLACES THE HTML FILE WITH THE SERIALIZED DATA
        """
        try:
            # Sort Movies Based on their ratings
            sorted_movies_data = dict(sorted(self._movies_data.items(),
                                             key=lambda item: float(item[1]['rating']),
                                             reverse=True))
        except ValueError:
            movies_to_remove = set()

            # check if any value is missing in the data. Missing values are assigned 'N/A'
            for movie in self._movies_data:
                for key, value in self._movies_data[movie].items():
                    if value == 'N/A':
                        # append all the keys with missing value in a list
                        movies_to_remove.add(movie)

            # remove all the data with missing value
            for movie in movies_to_remove:
                self._movies_data.pop(movie)

            # Sort Movies Based on their ratings
            sorted_movies_data = dict(sorted(self._movies_data.items(),
                                             key=lambda item: float(item[1]['rating']),
                                             reverse=True))

        # Serialize Movies for HTML
        movies_data_serialized = self._serialize_movie_data(sorted_movies_data)

        return self._replace_html_content(movies_data_serialized, self._html_content)

    def _serialize_movie_data(self, movies_data):
        """ GETS MOVIES DATA, SERIALIZES THE DATA FOR HTML """
        movies_html_output = ''
        for movie in movies_data:
            # Don't display image if no poster url
            if movies_data[movie]["image_url"].startswith('https'):
                # opening tag
                movies_html_output += '<li>\n\t<div class="movie">\n'
                # Movie id
                movies_html_output += '\t<a href="https://www.imdb.com/title/' \
                                      f'{movies_data[movie]["id"]}/" target="_blank">\n'
                # Movie poster url
                movies_html_output += '\t<img class="movie-poster" src="' \
                                      f'{movies_data[movie]["image_url"]}" title=""></a>\n'
                # Movie title and rating
                movies_html_output += f'\t<div class="movie-title">{movie} ' \
                                      f'[{movies_data[movie]["rating"]}]</div>\n'
                # Movie year
                movies_html_output += '\t<div class="movie-year">' \
                                      f'{movies_data[movie]["year"]}</div>'
                # Movie country flag
                movies_html_output += '\n<div class="movie-country">\n'
                movies_html_output += \
                    f'{self._get_country_flag(movies_data[movie]["country"])}' \
                    f'</div>'
                # Movie notes
                movies_html_output += '\n\t<div class="notes">' \
                                      f'{movies_data[movie]["note"]}</div>'
                # closing tags
                movies_html_output += '\n</div>\n</li>'
        return movies_html_output

    def _replace_html_content(self, html_serialized_output: str, html_content: str):
        """
        REPLACES PART OF THE ORIGINAL HTML CONTENT WITH THE SERIALIZED HTML OUTPUT,
        SAVES THE OUTPUT TO HTML FILE
        """

        # Replaces __TEMPLATE_TITLE__ and __TEMPLATE_MOVIE_GRID__ placeholders
        if '__TEMPLATE_TITLE__' and '__TEMPLATE_MOVIE_GRID__' in html_content:
            new_html_content = html_content \
                .replace('__TEMPLATE_TITLE__', self._html_file_title).replace(
                '__TEMPLATE_MOVIE_GRID__', html_serialized_output)
        else:
            searched_output = self._search_html_content(html_content)
            new_html_content = html_content.replace(searched_output,
                                                    html_serialized_output)
        self._save_html_file(new_html_content)
        print('successfully generated the website')

    def _search_html_content(self, html_content: str) -> str:
        """
        SEARCH FOR THE SPECIFIED PATTERN IN THE HTML FILE CONTENT.
        RETURNS THE SEARCHED OUTPUT AS A STRING
        """
        searched_string = ''
        searched_output = re.findall(self._search_pattern, html_content)
        for output in searched_output:
            searched_string += output
        return searched_string

    def _save_html_file(self, html_content):
        """ SAVES NEW HTML CONTENT TO HTML FILE """
        with open(self._html_file_path, 'w') as html_file:
            html_file.write(html_content)

    def _get_country_flag(self, country_name: str) -> str:
        """
        GETS THE COUNTRY NAME.
        IF TWO COUNTRY NAMES EXIST, SPLIT NAMES INTO EACH COUNTRY NAME.
        FETCH THE COUNTRY FLAG FOR EACH COUNTRY.
        RETURNS HTML IMG TAG CONTAINING THE FLAG URL
        """

        country_flag_output = ''
        # If more than one country exist, generate Html img element for each country
        if ',' in country_name:
            country_names = country_name.split(',')
            for country in country_names:
                country_flag_output += self._get_img_element(country)
        else:
            # Generates HTML img element for only one country
            country_flag_output += self._get_img_element(country_name)
        return country_flag_output

    def _get_img_element(self, country):
        """
        SEND GET REQUEST TO COUNTRY FLAG API,
        RETURNS IMG TAG WITH FLAG URL AND COUNTRY NAME.
        IF COUNTRY NOT FOUND, IT RETURNS ONLY COUNTRY NAME WITHOUT FLAG URL
        """
        payload_json = {"country": country.strip()}
        response = requests.post(self._country_flag_url, json=payload_json)
        if not response.json()['error']:
            return f'<img class="country-flag" src="{response.json()["data"]["flag"]}" ' \
                   f'alt="{country.strip()}">'
        else:
            return f'<img class="country-flag" src="{country.strip()}" ' \
                   f'alt="{country.strip()}">'

    def _validate_user_input(self, user_input):
        """
        GETS USER INPUT COMMAND
        VALIDATE THE COMMAND BY CHECKING THAT IT IS WITHIN THE RANGE 0-9
        RETURNS THE VALIDATED USER INPUT COMMAND
        """
        user_input = self._is_digit(user_input)
        while int(user_input) not in range(10):
            print(f"The option '{int(user_input)}' is not in the menu.")
            new_user_input = input(self._menu)
            user_input = self._is_digit(new_user_input)
        return user_input

    def _is_digit(self, user_input):
        """
        GETS USER INPUT COMMAND
        VALIDATE THE COMMAND BY CHECKING THAT USER COMMAND IS A NUMBER (OR DIGIT)
        RETURNS THE VALIDATED USER INPUT COMMAND
        """
        while not user_input.isdigit():
            user_input = input(self._menu)
        return user_input

    @property
    def _user_choice_command(self):
        """
        DISPATCH TABLE (DICT) THAT CONTAINS THE USER INPUT COMMANDS AS KEYS AND
        THE METHODS THAT EXECUTE THE COMMANDS AS VALUES.
        """
        return {0: '_command_exit_app',
                1: '_command_list_movies',
                2: '_command_add_movie',
                3: '_command_delete_movie',
                4: '_command_update_movie',
                5: '_command_movie_stats',
                6: '_command_random_movie',
                7: '_command_search_movies',
                8: '_sort_movies_by_rating',
                9: '_generate_website',
                }

    def run(self):
        """
        PRINTS THE MAIN MENU,
        GETS USER INPUT,
        CALLS THE REQUIRED METHOD AND EXECUTE COMMAND,
        EXITS APP WHEN USER QUITS
        """

        # Prints the heading of the Movies App
        print("\n** ** ** ** ** My Movies Database ** ** ** ** **")

        # Gets user input commands and validate the input commands
        user_input = input(self._menu)
        user_input_validated = self._validate_user_input(user_input)

        while user_input_validated:
            # Calls the method to execute user command and prints message to screen
            dispatcher = self._user_choice_command[int(user_input_validated)]
            getattr(self, dispatcher)()

            # Prompts user to press 'Enter' and checks that user presses the 'Enter' Key
            user = input("\nPress Enter to continue ")
            while user:
                user = input("\nPress Enter to continue ")

            # Gets user input commands and validate the input commands
            user_input = input(self._menu)
            user_input_validated = self._validate_user_input(user_input)

