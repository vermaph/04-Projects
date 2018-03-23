# importing required library 
import pandas as pd 
import os as os

from surprise import Dataset, evaluate
from surprise import KNNBasic

# setting working directory
path = 'C:/0000/05 Github/Codes/Pet Projects/Recommendation System - MovieLens/ml-100k'
os.chdir(path)  # sets working direcoty
os.listdir()    # lists all the fiels in the directory


#preparing the user file
user_col = ['user_id','age','gender','profession','zipcode']
users = pd.read_csv('u.user', sep = '|', header=None, names = user_col)


#preparing the ratings file
ratings_col = ['user_id','movie_id','rating','timestamp']
ratings = pd.read_csv('u.data', sep = '\t', header=None, names = ratings_col)


#preparing the movies file
movie_col = ['movie id',	'movie title',	'release date',	'video release date',	
             'IMDb URL',	'unknown',	'Action',	'Adventure',	'Animation',	
             'Childrens', 'Comedy', 	'Crime',	'Documentary',	'Drama',	'Fantasy',	
             'Film-Noir',	'Horror',	'Musical',	'Mystery',	'Romance',	
             'Sci-Fi',	'Thriller',	'War',	'Western'
             ]
movies = pd.read_csv('u.item',sep = '|', header = None, names = movie_col, encoding='latin-1')



#making training and testing datasets
train = pd.read_csv('ua.base', sep = '\t', header = None, names = ratings_col)
test = pd.read_csv('ua.test', sep = '\t', header = None, names = ratings_col)

## https://blog.dominodatalab.com/recommender-systems-collaborative-filtering/
data = Dataset.load_builtin("ml-100k")
trainingSet = data.build_full_trainset()

sim_options = {
    'name': 'cosine',

    'user_based': False
}
knn = KNNBasic(sim_options=sim_options)

knn.train(trainingSet)


testSet = trainingSet.build_anti_testset()



