'''
Created on Jun 12, 2013

@author: rdelavizaghbolagh@tudelft.net
'''

from numpy import *
#from scipy import *
import csv as csv 
import numpy as np
    
def readData(file):
    #The first thing to do is to import the relevant packages
    # that I will need for my script, 
    #these include the Numpy (for maths and arrays)
    #and csv for reading and writing csv files
    #If i want to use something from this I need to call 
    #csv.[function] or np.[function] first
    
   
    #Open up the csv file in to a Python object
    csv_file_object = csv.reader(open(file,'rb')) 
    header = csv_file_object.next()  #The next() command just skips the 
                                     #first line which is a header
    data=[]                          #Create a variable called 'data'
    for row in csv_file_object:      #Run through each row in the csv file
        data.append(row)             #adding each row to the data variable
    data = np.array(data)              #Then convert from a list to an array
                         #Be aware that each item is currently
                                     #a string in this format
    return data

if __name__ == '__main__':
    inputFile = '../../data/train.csv'
    data = readData(inputFile)
    print len(data)
    print data[0:2]
    print data[1::,0]