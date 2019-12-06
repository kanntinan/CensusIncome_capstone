# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 10:26:19 2019

@author: PromptNow
"""
import cv2, numpy as np, dlib, pickle

face_detector = cv2.CascadeClassifier('haarcascade_frontalface_default.xml') # face detection
detector = dlib.get_frontal_face_detector()
sp = dlib.shape_predictor('shape_predictor_68_face_landmarks.dat')
model =dlib.face_recognition_model_v1('dlib_face_recognition_resnet_model_v1.dat')
FACE_DESC, FACE_NAME = pickle.load(open('trainset.pk', 'rb')) #Load feature matrix
cap = cv2.VideoCapture(0) #Select camera

while True:
    _, frame = cap.read()
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    faces = face_detector.detectMultiScale(gray, 1.1, 4)
    for (x, y, w, h) in faces:
        img = frame[y-10:y+h+10, x-10:x+w+10][:,:,::-1]
        dets = detector(img, 1)
        for k, d in enumerate(dets):
            shape = sp(img, d)
            face_desc0 = model.compute_face_descriptor(img, shape, 100)
            d = [] #List to store distance
            for face_desc in FACE_DESC:
                d.append(np.linalg.norm(np.array(face_desc) - np.array(face_desc0))) #NN algorithm
            d = np.array(d)  #store in array
            idx = np.argmin(d) #find min value
            if d[idx] < 0.5: #set threshold
                name = FACE_NAME[idx] #Return index name
                print(name)
                cv2.putText(frame, name, (x, y - 5), cv2.FONT_HERSHEY_COMPLEX, .7, (255,255,255),2)
                cv2.rectangle(frame, (x,y), (x+w,y+h), (255,0,0), 2)
    
    cv2.imshow('frame', frame)
    cv2.waitKey(1) #Input 0 for picture or 1 for video
