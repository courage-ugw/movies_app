import random
import re
import statistics

from fuzzywuzzy import process
from movie_storage import list_movies, add_movie, update_movie, delete_movie
from movie_utils import load_json_file, create_file, get_country_flag

EXIT_APP = 0
APP_TITLE = "\n** ** ** ** ** My Movies Database ** ** ** ** **"
HTML_TEMPLATE_TITLE = "Masterschool's Movie App"
HTML_TEMPLATE_FILE = '_static/index_template.html'
SEARCH_PATTERN = '<li>[\s\S]*?<div class="movie">\s*?<a href=".*?" target="_blank">\s*?' \
                 '<img class="movie-poster" src=".*?" title=""><\/a>\s*?' \
                 '<div class="movie-title">.*?<\/div>\s*?<div class="movie-year">.*?' \
                 '<\/div>[\s\S]*?<\/li>'

# DISPLAYS MENU TO THE USER
USER_CHOICE_STR: str = """ 
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
# Dispatch Table from Menu
USER_CHOICE_DICT: dict[int, str] = {
    1: 'list_movies',
    2: 'add_movie',
    3: 'delete_movie',
    4: 'update_movie',
    5: 'movie_stats',
    6: 'random_movie',
    7: 'search_movies',
    8: 'movies_sorted_by_rating',
    9: 'generate_website',
}


def app_menu() -> None:
    """ DISPLAYS THE MAIN MENU, GETS USER CHOICE INPUT, CALLS THE REQUIRED FUNCTION,
    EXITS APP WHEN USER QUITS """

    # CREATE FILE IF FILE DOES NOT EXIT
    create_file()
    try:
        user_input = int(input(USER_CHOICE_STR))
    except ValueError:
        user_input = input(USER_CHOICE_STR)
        # CHECKS IF USER ENTERED A NUMBER
        user_input = user_input_is_numeric(user_input)

    # PROMPTS USER TO RE-ENTER NUMBER IF NUMBER IS OUTSIDE THE RANGE: 0-9
    while user_input not in range(10):
        print(f"The option '{user_input}' is not in the menu.")
        try:
            user_input = int(input(USER_CHOICE_STR))
        except ValueError:
            user_input = input(USER_CHOICE_STR)
            # CHECKS IF USER ENTERED A NUMBER
            user_input = user_input_is_numeric(user_input)

    while user_input is not EXIT_APP:
        # DISPATCHES THE ACTIONS TO BE PERFORMED BASED ON USER CHOICE
        dispatcher = eval(USER_CHOICE_DICT[int(user_input)])
        # CALLS THE FUNCTION TO EXECUTE USER CHOICE
        output = dispatcher()
        # DISPLAYS MESSAGE TO THE USER AFTER ACTION HAS BEEN PERFORMED
        display_message(output)
        user = input("\nPress Enter to continue ")

        # ENSURES THAT USER PRESSES ENTER ON THE KEYBOARD
        if user:
            while user != "":
                user = input("\nPress Enter to continue ")
        try:
            user_input = int(input(USER_CHOICE_STR))
        except ValueError:
            user_input = input(USER_CHOICE_STR)
            # CHECKS IF USER ENTERED A NUMBER
            user_input = user_input_is_numeric(user_input)

    # DISPLAY MESSAGE WHEN USER EXITS  APP
    print('Bye!')


def user_input_is_numeric(user_input: str) -> int:
    """ CHECKS IF USER ENTERED A NUMBER, RETURNS USER INPUT ONLY WHEN
    USER ENTERED A NUMBER """

    while not user_input.isnumeric():
        user_input = input(USER_CHOICE_STR)
    return int(user_input)


def display_message(output: str) -> None:
    """ DISPLAYS MESSAGES TO THE USER AFTER USER CHOICE ACTION HAS BEEN PERFORMED """
    if isinstance(output, str):
        print(output)
    elif isinstance(output, dict):
        print(f'{len(output)} movies in total: ')
        for item in output:
            print(f"{item}: {output[item][0]}, {output[item][1]}")
    else:
        for item in output:
            print(f'{item[0]}: {item[1]}')


def movie_stats() -> str:
    """ LOADS JSON FILE, GETS MOVIE NAMES AND RATINGS, CALLS THE CALCULATE MOVIES
     FUNCTION AND RETURNS MOVIE STATS """
    movies_data = load_json_file()
    movies = movie_names_and_ratings(movies_data)
    return calculate_movie_stats(movies)


def movie_names_and_ratings(movies_data: dict) -> dict:
    """ GETS MOVIES DICT DATA, RETURNS ONLY MOVIE NAMES AND RATINGS """
    movies = {movie: float(movies_data[movie]['rating']) for movie in movies_data}
    return movies


def calculate_movie_stats(movies: dict) -> str:
    """ GETS MOVIES DATA AS DICT, CALCULATES MEAN AND MEDIAN STATISTICS ON THE DATA USING
    THE RATINGS AND RETURNS THE MEAN, MEDIAN AND BEST MOVIES BASED RATINGS """

    average_rating = statistics.mean(movies.values())
    median_rating = statistics.median(movies.values())
    movies_sorted = sorted(movies.items(), key=lambda item: item[1],
                           reverse=True)
    best_movie, best_rating = movies_sorted[0]
    worst_movie, worst_rating = movies_sorted[-1]
    return f"""
Average rating: {average_rating :.2f}
Median rating: {median_rating}
Best Movie: {best_movie}, {best_rating}
Worst Movie: {worst_movie}, {worst_rating}
"""


def random_movie() -> str:
    """ LOAD DICT DATA FROM JSON FILE, GETS ONLY MOVIE NAMES AND RATINGS, USE
     THE RANDOM FUNCTION TO GET A RANDOM MOVIE NAME AND RATING AND RETURN THEM """

    movies_data = load_json_file()
    movies = movie_names_and_ratings(movies_data)
    movie, rating = random.choice(list(movies.items()))
    return f"Your movie for tonight: {movie}, it's rated {rating}"


def search_movies() -> str:
    """ GETS MOVIE NAME OF OR PART OF THE MOVIE NAME TO SEARCH FOR, APPLIES NAME
     COMPARISON WITH FUZZYWUZZY MODULE, AND RETURNS THE MOVIE NAME THAT IS BEING
     SEARCHED FOR ALONGSIDE ITS RATING """

    movie_name = input('Enter part of movie name: ')
    movies_data = load_json_file()
    movies = movie_names_and_ratings(movies_data)
    movies_lowercase = {key.lower(): value for key, value in movies.items()}
    try:
        name, match_score = process.extractOne(movie_name, movies_lowercase.keys(),
                                               score_cutoff=70)
    except TypeError:
        return f'The string "{movie_name}" does not match any movie name!'
    if name:
        for movie, rating in movies_lowercase.items():
            if name.lower() == movie:
                return f"{movie.title()}, {rating}"


def movies_sorted_by_rating() -> list:
    """ LOADS DICT DATA FROM JSON FILE, GETS ONLY THE MOVIE NAMES AND
    THEIR RATINGS, SORTS DATA BASED ON RATINGS. RETURNS THE SORTED DATA AS LIST """
    movies_data = load_json_file()
    movies = movie_names_and_ratings(movies_data)
    movies_sorted = sorted(movies.items(), key=lambda item: item[1], reverse=True)
    return movies_sorted


def generate_website() -> str:
    """ LOADS CONTENT FROM HTML FILE, LOADS MOVIES DATA FROM JSON FILE AND SERIALIZES
     THE DATA FOR HTML. REPLACES THE HTML FILE WITH THE SERIALIZED DATA """
    html_content = load_html_file()
    movies_data = load_json_file()
    movies_data_serialized = serialize_movie_data(movies_data)
    return replace_html_content(movies_data_serialized, html_content)


def load_html_file() -> str:
    """ LOADS HTML FILE AND RETURNS THE CONTENT AS STRING """
    with open(HTML_TEMPLATE_FILE, 'r') as html_file:
        return html_file.read()


def serialize_movie_data(movies_data: dict) -> str:
    """ GETS MOVIES DATA, SERIALIZES THE DATA FOR HTML """
    movies_html_output = ''
    for movie in movies_data:
        movies_html_output += '<li>\n\t<div class="movie">\n'
        movies_html_output += '\t<a href="https://www.imdb.com/title/' \
                              f'{movies_data[movie]["id"]}/" target="_blank">\n'
        movies_html_output += f'\t<img class="movie-poster" src="' \
                              f'{movies_data[movie]["image_url"]}" title=""></a>\n'
        movies_html_output += f'\t<div class="movie-title">{movie} ' \
                              f'[{movies_data[movie]["rating"]}]</div>\n'
        movies_html_output += f'\t<div class="movie-year">' \
                              f'{movies_data[movie]["year"]}</div>'
        movies_html_output += '\n<div class="movie-country">'
        movies_html_output += f'\n{get_country_flag(movies_data[movie]["country"])}</div>'
        movies_html_output += f'\n\t<div class="notes">' \
                              f'{movies_data[movie]["note"]}</div>'
        movies_html_output += f'\n</div>\n</li>'
    return movies_html_output


def replace_html_content(html_serialized_output: str, html_content: str) -> str:
    """ REPLACES PART OF THE ORIGINAL HTML CONTENT WITH THE SERIALIZED HTML OUTPUT
     AND SAVES THE OUTPUT TO HTML FILE"""

    # REPLACES MOVIE TITLE AND MOVIE GRID PLACEHOLDERS
    if '__TEMPLATE_TITLE__' and '__TEMPLATE_MOVIE_GRID__' in html_content:
        html_content_replaced = html_content.replace('__TEMPLATE_TITLE__',
                                                     HTML_TEMPLATE_TITLE).replace(
            '__TEMPLATE_MOVIE_GRID__',
            html_serialized_output)
    else:
        searched_output = search_html_content(html_content)
        html_content_replaced = html_content.replace(searched_output,
                                                     html_serialized_output)
    save_html_file(html_content_replaced)
    return 'successfully generated the website'


def search_html_content(html_content: str) -> str:
    """ SEARCH FOR THE SPECIFIED PATTERN IN THE HTML FILE CONTENT.
      RETURNS THE SEARCHED OUTPUT AS A STRING """
    searched_string = ''
    searched_output = re.findall(SEARCH_PATTERN, html_content)
    for output in searched_output:
        searched_string += output
    return searched_string


def save_html_file(html_content: str) -> None:
    """ SAVES THE HTML TO HTML FILE """
    with open(HTML_TEMPLATE_FILE, 'w') as html_file:
        html_file.write(html_content)


def main():
    """ PRINTS THE APP NAME, AND CALLS THE MAIN MENU FUNCTION """
    print(APP_TITLE)
    app_menu()


if __name__ == "__main__":
    main()
