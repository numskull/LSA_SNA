#import chardet

import pandas as pd
import numpy
from numpy import linalg
from operator import itemgetter

# with open('shakespeare.csv', 'r') as f:
#     result = chardet.detect(f.readline())
df = pd.read_csv('shakespeare.csv', delimiter=',', encoding='latin-1')
#print(df['Unnamed: 0'])
df2 = df.set_index('Unnamed: 0')
#print(df2['1H4'])
#print("Sum Cols: " + str(sum(df2.sum(axis=1))))
# love = df2.loc['love',]
# well = df2.loc['well']
#
# prod = love * well
#print(prod)
#print(df2.loc[df2.index[16]] == df2.loc['man'])



def euclidDist(df, term):
    termVec = df.loc[str(term)] #Get term row
    distances = {}
    for index, r in df.iterrows():
        dist = 0
        #print(distances)
        for j in range(1,len(r)):
            #print(index)
            if term != index:
                dist = dist + pow(r[j] - termVec[j], 2)
        distances[str(index)] = numpy.sqrt(dist)
    return(distances)

loveDistances = euclidDist(df2, 'love')
topLove = sorted(loveDistances.items(), key=itemgetter(1))[1:10]
loveEarth = loveDistances['earth']
print(loveEarth)
for key, value in topLove:
    print(key + " " + str(value))

def cosineDist(df, term):
    termVec = df.loc[str(term)] #Get term row
    distances = {}
    termDot = numpy.sqrt(sum(termVec*termVec))
    for index, r in df.iterrows():
        dist = 0
        rowDot = numpy.sqrt(sum(r*r))
        if term != index:
            dist = dist + (sum(r * termVec)/(rowDot*termDot))
        if dist != 0:
            distances[str(index)] = numpy.sqrt(dist)
    return(distances)

loveCosine = cosineDist(df2, 'love')
topLove = sorted(loveCosine.items(), key=itemgetter(1), reverse=True)[1:10]
print("Cosine Distance")
for key, value in topLove:
    print(key + " " + str(value))

def ppmi(df, smooth=False):
    ppmiDf = df.copy()
    rowSums = sum(ppmiDf.sum(axis=0))
    colSums = sum(ppmiDf.sum(axis=1))
    bottom = rowSums + colSums
    for index, r in df.iterrows():
        sumR = sum(r)
        for colName, col in df.iteritems():
            sumJ = sum(col)
            pij = (df.loc[index,colName] / bottom)
            pi = (sumR/bottom)
            pj = (sumJ / bottom)
            #print(str(pij) + " " + str(pi) + " " + str(pj))
            if pij == 0:
                pmi = 0
            else:
                pmi = numpy.log(pij / (pi * pj))
            if pmi < 0:
                pmi = 0
            if smooth and pmi !=0:
                pmi = smoothPpmi(df.loc[index, colName], pmi, sumR, sumJ)
            ppmiDf.loc[index, colName] = pmi
    return(ppmiDf)

def smoothPpmi(f, pmi, sumR, sumJ):
    sij = (f/(f+1)) * (min(sumR, sumJ) / (min(sumR, sumJ) + 1))
    return(sij * pmi)

def bellNorm(df):
    normDf = df.copy()
    for rowName, r in df.iterrows():
        ti = sum(r)
        for colName, col in df.iteritems():
            #print(rowName + " " + colName + ": " + str(df.loc[rowName,colName]))
            if df.loc[rowName,colName] != 0:
                ei = -(1/log(len(df.columns)))*sum((df.loc[rowName, colName]/ti)*log(df.loc[rowName,colName]/ti))
                #print("EI...:::...:::::>: " + str(ei))
                w = (1-ei)*(df.loc[rowName,colName] / (len(col)))
                #print(str(normDf.loc[rowName, colName]))
                normDf.loc[rowName,colName] = w
    return normDf

def tfidf(df):
    tfdf = df.copy()

    for rowName, r in df.iterrows():
        for colName, col in df.iteritems():
            tf = df.loc[rowName, colName]
            idf = log(len(r)/len(r.nonzero()))
            tfidf = tf/idf
            tfdf.loc[rowName, colName] = tfidf
    return tfdf

def comparisons(u,s,v,termOne, termTwo, what="u"):
    s = s.copy()
    if what == "u":
        u = u.copy()
    elif what == "v":
        u = v.copy()
    elif what == "uv":
        u = u.copy()
        v = v.copy()

    ui = u.loc[termOne,:]

    #print(str(s))
    uj = u.transpose().loc[:,termTwo]
    print(str(uj.shape))
    #print("UI ::::> " + str(type(ui)) + "\nUJ :::::> " + str(type(uj)))
    uiDot = numpy.sqrt(ui.dot(s).dot(ui.dot(s)))
    print("UIDOT:::> " + str(ui.shape))
    ujDot = numpy.sqrt(uj.dot(s).dot(uj.dot(s)))
    print(str(uj.dot(s.pow(2))))
    cos = ((ui.dot(s.pow(2))*sum(uj)) / (uiDot*ujDot))
    return(cos)

# ppmiDf = ppmi(df2)
# ppmiDf.to_csv('ppmi.csv')
# print("Post PPMI:")
# lovePpmiDist = euclidDist(ppmiDf, 'love')
# topLove = sorted(lovePpmiDist.items(), key=itemgetter(1))[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
#
# lovePpmiCosine = cosineDist(ppmiDf, 'love')
# topLove = sorted(lovePpmiCosine.items(), key=itemgetter(1), reverse=True)[1:10]
# print("Cosine Distance")
# for key, value in topLove:
#     print(key + " " + str(value))
#
#
# ppmiDf = ppmi(df2, True)
# ppmiDf.to_csv('ppmi_smoothed.csv')
# print("Post PPMI Smoothing:")
# lovePpmiDist = euclidDist(ppmiDf, 'love')
# topLove = sorted(lovePpmiDist.items(), key=itemgetter(1))[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
#
# lovePpmiCosine = cosineDist(ppmiDf, 'love')
# topLove = sorted(lovePpmiCosine.items(), key=itemgetter(1), reverse=True)[1:10]
# print("Cosine Distance")
# for key, value in topLove:
#     print(key + " " + str(value))
#
#
#
# print("DF2::::::: " + str(df2.loc['thou', '1H4']))
# newNorm = bellNorm(df2)
# newNorm.to_csv("bell_norm.csv")
#
# print("\nPost Bell Norm:")
# loveBellDist = euclidDist(newNorm, 'love')
# topLove = sorted(loveBellDist.items(), key=itemgetter(1))[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
#
# print("\nBell Dist Cosine: ")
# loveBellCosine = cosineDist(newNorm, 'love')
# topLove = sorted(loveBellCosine.items(), key=itemgetter(1), reverse=True)[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
#
# tfdf = tfidf(df2)
# tfdf.to_csv("tfidf.csv")
# print("\nTFIDF Values::: ")
# print("\ntfidf Norm:")
# loveBellDist = euclidDist(tfdf, 'love')
# topLove = sorted(loveBellDist.items(), key=itemgetter(1))[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
#
# print("\ntfidf Cosine: ")
# loveBellCosine = cosineDist(tfdf, 'love')
# topLove = sorted(loveBellCosine.items(), key=itemgetter(1), reverse=True)[1:10]
# for key, value in topLove:
#     print(key + " " + str(value))
# df3 = pd.read_csv('test.csv', delimiter=',', encoding='latin-1')
# U, S, V = linalg.svd(df3, full_matrices=False)
# U = pd.DataFrame(U)#, index=df2.index)
# S = pd.DataFrame(S)
# V = pd.DataFrame(V)#, columns=df2.columns)
# print("W::::> \n" + str(df3))
# print("Wt:::> \n" + str(df3.transpose()))
# dfT = df3.transpose
# dfTrans = df3 * dfT
# print("W * Wt :>>>>\n" + str(dfTrans))
# dfTrans.to_csv("wwt.csv")
# test = U.dot(S).dot(S).dot(U.transpose)
# print(str(test))

#print(dfTrans == test)
# U, S, V = linalg.svd(df2, full_matrices=False)
# U = pd.DataFrame(U, index=df2.index)
# S = pd.DataFrame(S)
# V = pd.DataFrame(V, columns=df2.columns)
# cos = comparisons(U, S, V, 'king', 'love')
# print(cos)
#print(str(cos))
#U.to_csv("u.csv")
#S.to_csv("s.csv")
#V.to_csv("v.csv")
# with open("shakespeare.csv", 'r') as f:
#     m = list(csv.reader(f, delimiter=","))
#
# print(m[0:2])
