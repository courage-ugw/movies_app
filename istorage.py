from abc import ABC, abstractmethod


class IStorage(ABC):
    """
    AN INTERFACE (ABSTRACT CLASS) THAT EXPOSES ALL THE CRUD COMMANDS (ABSTRACT METHODS,
    WHICH ARE TO BE IMPLEMENTED BY THE SUBCLASSES)
    """

    @abstractmethod
    def list_movies(self):
        """
           Returns a dictionary of dictionaries that
           contains the movies_manager information in the db_managers.

           The function loads the information from the file
           and returns the data.

           For example, the function may return:
           {
             "Titanic": {
               "rating": 9,
               "year": 1999
             },
             "..." {
               ...
             },
           }
           """

    @abstractmethod
    def add_movie(self, title):
        """
       Adds a movie to the movies_manager db_managers.
       Loads the information from file, add the movie,
       and saves it.
       """

    @abstractmethod
    def delete_movie(self, title):
        """
       Deletes a movie from the movies_manager db_managers.
       Loads the information from file, deletes the movie,
       and saves it.
       """

    @abstractmethod
    def update_movie(self, title, notes):
        """
       Updates a movie from the movies_manager db_managers.
       Loads the information from file, updates the movie,
       and saves it
       """
