# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 11:42:32 2019
@author: PromptNow
"""

# Create Feature vector
import numpy as np, cv2, dlib, os, pickle
path = './facedata/'
detector = dlib.get_frontal_face_detector() #Build detector
sp = dlib.shape_predictor('shape_predictor_68_face_landmarks.dat') #Landmark detection
model =dlib.face_recognition_model_v1('dlib_face_recognition_resnet_model_v1.dat') #Read model

FACE_DESC = []
FACE_NAME = []
for fn in os.listdir(path): #fine file name in path folder
    if fn.endswith('.jpg'): #find file that end with .jpg
        img = cv2.imread(path + fn)[:,:,::-1] #read picture, change to RGB
        dets = detector(img, 1) #Parameter '1' is resolution
        for k, d in enumerate(dets): #fine all picture from detect phase
            shape = sp(img, d) #landmark detect all picture
            face_desc = model.compute_face_descriptor(img, shape, 1) #Add picture to model and jitter parameter
            FACE_DESC.append(face_desc) #store matrix to FACE_DESC
            print('loading...', fn)
            FACE_NAME.append(fn[:fn.index('_')]) #set name output
pickle.dump((FACE_DESC, FACE_NAME), open('trainset.pk', 'wb')) #store variables

