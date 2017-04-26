% PROTOTYPE IMPLEMENTATION FOR EbH, a MECHANISM TO DERIVE TIME-INVARIANT
% SYMMETRIC ENCRYPTION KEYS FROM ECG
% Version 0.6
% Authors (script): L. Gonzalez-Manzano, J. M. de Fuentes
% Authors (paper): L. Gonzalez-Manzano, J. M. de Fuentes, P. Peris-Lopez, C. Camara
% CARLOS III UNIVERSITY OF MADRID (SPAIN)

% CITATION DETAILS
% Paper citation will be included here upon acceptance of the paper

% This script contains a subset of instructions obtained from
% Stackoverflow. When this happens, the source is cited.

% USAGE INSTRUCTIONS
% This script is to be run over ECG data that has been pre-processed using
% a Walsh-Hadamard Transform following the steps described in the paper.
% Results reported in the paper were obtained by running this script on
% pre-processed dataset E-HOL-03-202-003, provided by the Telemetric and
% ECG Warehouse (THEW) of University of Rochester

% OUTPUT
% This script produces an Excel spreadsheet as output. The name is based on
% the actual date, followed by a random suffix. The spreadsheet contains
% all parameters discussed in the paper

%clean all vars
clearvars
%Amount of ECG samples per subject
samples=360;
%Amount of attributes per ECG signal
numVarS=194;
%This file should contain the pre-processed ECG values as described above
%Such a pre-processing should consider the values for L_a and L_w as
%described in the paper.
load 'preprocessed.mat'
%Amount of subjects
NUM_INDIV = 199;
%Amount of ECG samples taken to build the subject model ECG_Mod
TRAININGS = [0.2 0.6];
%Amount of ECG samples to compute the user secret value (i.e. parameter L_O
%in the paper)
SIZES_OBSERVED=[3 10];


%Tolerance value for each attribute value, to consider that it fits the
%subject model
TOLERANCE=0;
%Percentage of the minimum value that will be taken to compute TOLERANCE
%This is the TOLERANCE MARGIN (TM) parameter of the paper
TOLERANCE_FACTORS =[0.01 0.05 0.1 0.2];
%Minimum value that an attribute in the subject model must have in order to
%consider it in the user secret value computation. Otherwise, the attribute
%is discarded
DISCARD_ATT_VALUE=0;
%Percentage of the maximum value that will be taken to compute
%DISCARD_ATT_VALUE
%This is the DISCARD THRESHOLD (DT) parameter of the paper
DISCARD_FACTORS = [0.005 0.01 0.03 0.05];

filename=char(strcat('results',date(),'-',string(rand()),'.xlsx'));
disp(sprintf(filename));
javaaddpath('./poi_library/poi-3.8-20120326.jar');
javaaddpath('./poi_library/poi-ooxml-3.8-20120326.jar');
javaaddpath('./poi_library/poi-ooxml-schemas-3.8-20120326.jar');
javaaddpath('./poi_library/stax-api-1.0.1.jar');
javaaddpath('./poi_library/xmlbeans-2.3.0.jar');
javaaddpath('./poi_library/dom4j-1.6.1.jar');
%LOOP to apply each of the sizes for the training set (i.e. ECG_Mod)
for currTrain = 1:size(TRAININGS,2)
    TRAINING = TRAININGS(currTrain);
    %LOOP to apply each of the values for parameter L_o in the paper
    for currSizeObs = 1:size(SIZES_OBSERVED,2)
        SIZE_OBSERVED = SIZES_OBSERVED(currSizeObs);


        %LOOP to apply each of the values for parameter TM in the paper
        for currTol = 1:size(TOLERANCE_FACTORS,2)
            TOLERANCE_FACTOR=TOLERANCE_FACTORS(currTol);
            %LOOP to apply each of the values for parameter DT in the paper
            for currDisc = 1:size(DISCARD_FACTORS,2)
                DISCARD_FACTOR=DISCARD_FACTORS(currDisc);
                %Clean all loop-dependent vars
                clearvars -except filename NUM_INDIV Ototal Ototal2 samples numVarS currTrain currSizeObs currTol currDisc TRAININGS TRAINING SIZES_OBSERVED SIZE_OBSERVED TOLERANCE_FACTORS TOLERANCE_FACTOR DISCARD_FACTORS DISCARD_FACTOR
                TOLERANCE=0;
                DISCARD_ATT_VALUE=0;        
                %Variables of the first subject are extracted
                T=1;
                disp(sprintf('TRAIN= %.2f SIZEOBS = %d TOLERANCE = %.4f DISCARD = %.4f',TRAINING, SIZE_OBSERVED, TOLERANCE_FACTOR, DISCARD_FACTOR));
                %%% PROCEDURE TO CHECK ALL SUBJECTS, INCLUDING COMPUTING THE MODEL AND THE
                %%% SECRET VALUE FOR ALL COMPUTATION
                for S=1:NUM_INDIV
                    %GET SAMPLES FOR INDIVIDUAL S
                    paramSubjectS=squeeze(Ototal2(S,:,:));
                    %Training set (i.e. ECG_Mod): portion of paramSubjectS
                    trainingSubjectS=paramSubjectS(1:ceil(samples*TRAINING),:);
                    for i=1:numVarS
                        totalTrainS(i) = sum(trainingSubjectS(:,i));
                    end
                    %totalTrainS: average of value for each attribute in the training set
                    totalTrainS = totalTrainS/size(trainingSubjectS,1);
                    DISCARD_ATT_VALUE = DISCARD_FACTOR * max(abs(totalTrainS));
                    TOLERANCE= TOLERANCE_FACTOR * mean(abs(totalTrainS));
                    %testSubjectS: test set for each subject. This will be the set of
                    %samples used to compute the user secret value
                    testSubjectS=paramSubjectS((ceil(samples*TRAINING)+1):samples,:);
                    %The next lines go through all the test set. timesToTest say how many
                    %times a user secret value will be computed based on the amount of
                    %samples and the size of the set that is used each time
                    timesToTest = (samples - (ceil(samples*TRAINING)+1) ) / SIZE_OBSERVED;
                    count=1;

                    for test = 1:timesToTest
                        %take the set of samples to compute the user secret value
                        testSamples = testSubjectS(count:(count+SIZE_OBSERVED-1),:);
                        distinctUserSecretValueWithCounter(test, 1) = 1;
                        for j=1:numVarS
                            %totalTest : average value for each attribute in the set of ECG
                            %values observed
                            totalTest(j)=sum(testSamples(:,j))/size(testSamples,1);
                            % USER SECRET VALUE COMPUTATION FOR EACH
                            % ATTRIBUTE (see Equations 4 and 5 in the
                            % paper)
                            % 1. If the attribute value in the user model is below a given
                            % limit, discard the value
                            if abs(totalTrainS(j))<=DISCARD_ATT_VALUE
                                userSecretValues(test,j) = 9;
                                distinctUserSecretValueWithCounter(test, j+1) = 9;
                            else
                                %2. For those attributes which are over the limit, the user
                                %value is:
                                % 1 (resp. -1), if both values are positive (resp. negative) and the value in the test 
                                %is "near enough" to the value in the user model
                                % 0 otherwise
                                %disp(sprintf('totalTest(j)= %f , belongs to ( %f , %f)?',totalTest(j), (totalTrainS(j)*(1-TOLERANCE)), (totalTrainS(j)*(1+TOLERANCE))));
                                if totalTrainS(j) >= 0
                                    if (totalTest(j) >= (totalTrainS(j)*(1-TOLERANCE)) && (totalTrainS(j)*(1+TOLERANCE)) > totalTest(j))
                                        userSecretValues(test, j) = 1;
                                        distinctUserSecretValueWithCounter(test, j+1) = 1;
                                    else
                                        userSecretValues(test, j) = 6;
                                        distinctUserSecretValueWithCounter(test, j+1) = 6;
                                    end
                                else
                                    %totalTrainS <0 !
                                    if (totalTest(j) >= (totalTrainS(j)*(1+TOLERANCE)) && (totalTrainS(j)*(1-TOLERANCE)) > totalTest(j))
                                        userSecretValues(test,j) = -1;
                                        distinctUserSecretValueWithCounter(test, j+1) = -1;
                                    else
                                        userSecretValues(test,j) = 6;
                                        distinctUserSecretValueWithCounter(test, j+1) = 6;
                                    end
                                end
                            end
                        end

                %%%% CHECK QUALITY OF RESULTS (ASSESSMENT) 
                %%%% 1. CHECK HOW MANY USER SECRET VALUES ARE THE SAME
                        for i=1: test-1
                            if isequal(userSecretValues(i,:),userSecretValues(test,:))
                                distinctUserSecretValueWithCounter(i,1)=distinctUserSecretValueWithCounter(i,1)+1;
                                distinctUserSecretValueWithCounter(test,:)=0;
                            end
                        end     

                        count=count+SIZE_OBSERVED;
                    end
                    %2. MEASURE THE SIZE OF THE PRODUCED SEEDS
                    zero(1:numVarS)=0;
                    %zerosAndOnes is the table that stores for each key the amount of
                    %1,0,-1 
                    %This will be the "title" of the table
                    zerosAndOnes(1,2)=1;
                    zerosAndOnes(1,3)=0;              
                    zerosAndOnes(1,4)=-1;
                    for i=1:timesToTest
                         A=distinctUserSecretValueWithCounter(i,:);
                         if not(isequal(A(2:end),zero(1:end)))
                             %This is the line in which user secret values are copied to a
                             %final matrix in which all secret values for all users are
                             %copied in finalResult!
                             finalResult(T,:)=[100+S,A];
                              T=T+1;
                              sizeA = size(A(2:end));
                              num1=0;
                              numMinus1=0;
                              num0=0;

                              for t=1:(sizeA(1,2)+1) 
                                 if A(1,t)==1
                                     num1=num1+1;
                                 elseif A(1,t)==-1
                                     numMinus1=numMinus1+1;
                                 elseif A(1,t)==6
                                     num0=num0+1;
                                 end
                              end
                              zerosAndOnes(T,1)=100+S;
                              zerosAndOnes(T,2)=num1;
                              zerosAndOnes(T,3)=num0;              
                              zerosAndOnes(T,4)=numMinus1;
                         end

                    end
                   
                end

                %3. CHECK IF THERE ARE REPEATED ROWS AMONG USERS
                % This would be undesirable, since two users would have the
                % same seed
                X=unique(finalResult(:,3:end),'rows');
                sizeX=size(X);
                sizeXfinalResult=size(finalResult(:,3:end));
                if sizeX(1,1) == sizeXfinalResult(1,1)
                    disp('All user seeds are unique, everything is OK!');
                    disp(sprintf('Number of unique user secret values: %d',sizeX(1,1) ));
                    xlwrite(filename,sizeX(1,1),char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+currTol),string(1+currDisc+5*currSizeObs))));
                    
                else
                    disp('Equal user seed among users -- PROBLEM!');
                    disp(sprintf('Number of user secret values in total: %d', sizeXfinalResult(1,1)) );
                     disp(sprintf('Number of unique user secret values: %d',sizeX(1,1) ));
                     xlwrite(filename,sizeXfinalResult(1,1)+0.99,char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+currTol),string(1+currDisc+5*currSizeObs))));
                end
                avgValues= sum(sum(zerosAndOnes(2:end,2:end)))/(size(zerosAndOnes,1)-1);
                disp(sprintf('Average size per secret: %.6f',avgValues));
                xlwrite(filename,avgValues,char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+size(DISCARD_FACTORS,2)+currTol+1),string(1+currDisc+5*currSizeObs))));
                
                standDevia = std2(zerosAndOnes(2:end,2:end));
                disp(sprintf('Standard deviation per secret: %.6f',standDevia));

                %4. CHECK MIN-ENTROPY
                % MATRIX finalResult contains all user keys. Each column can be seen as a random variable, with different probabilities of having -1,0,1 or 9 values. 
                % By definition, the most conservative way of measuring entropy is the min-entropy: sum of negative logarithm of the max probability. 
                % As each random variable is independent from the others, it can be calculated as the sum
                optimalMinEntropy= numVarS * (-log10(0.25));
                % Let us calculate the min Entropy:
                actualMinEntropy = 0;
                for a=1:(size(finalResult,2)-2)
                    % taken from: http://stackoverflow.com/questions/6933658/matlab-computing-the-probability-of-each-element-within-a-vector
                    singleAttValues=finalResult(:,a+2);
                    probSingleAttValues = arrayfun(@(x)length(find(singleAttValues==x)), unique(singleAttValues)) / length(singleAttValues);
                    actualMinEntropy= actualMinEntropy + (- log(max(probSingleAttValues)));
                end
               
                disp(sprintf('Current min-Entropy: %.2f',actualMinEntropy ));
                xlwrite(filename,actualMinEntropy,char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+2*(size(DISCARD_FACTORS,2)+1)+currTol),string(1+currDisc+5*currSizeObs))));
                %PROBABILITY OF NO DECRYPTION AND ATTEMPTS TO DECRYPT
                %CALCULATION
                userKeyTimes=finalResult(:,1:2);
                attemptsToDecrypt=0;
                probNoDecrypt=0;
                numeratorAttempts=0;
                %Displaying the amount of users that
                %have one key (i.e. perfect time-invariance)
                perfectUsers=0;
                for i=1:NUM_INDIV
                    %if user 100+1 only has one key, then it always decrypts and it takes
                    %one attempt to decrypt always
                    countKeysUser = sum(userKeyTimes(:,1)==100+i);
                    if countKeysUser == 1
                        perfectUsers=perfectUsers+1;
                        attemptsToDecrypt=attemptsToDecrypt+1;
                    else
                       %the user has more than one key
                       keyGroupsUser = userKeyTimes(userKeyTimes==100+i,2);
                       if ismember(1,keyGroupsUser)
                           %at least one key appears just once
                           probNoDecrypt=probNoDecrypt + sum(keyGroupsUser==1)/sum(keyGroupsUser);
                       end
                       for cnt=1:size(keyGroupsUser,1)
                           if keyGroupsUser(cnt) ~= 1
                             numeratorAttempts = numeratorAttempts + ( keyGroupsUser(cnt) * ((keyGroupsUser(cnt)-1)/(sum(keyGroupsUser)-1)));  
                           end
                       end
                       attemptsToDecrypt=attemptsToDecrypt + (numeratorAttempts/(sum(keyGroupsUser)-sum(keyGroupsUser==1)));

                     
                       
                    end
                end
                avgAttemptsToDecrypt=attemptsToDecrypt/NUM_INDIV;
                avgProbNoDecrypt = probNoDecrypt/NUM_INDIV;
                disp(sprintf('Amount of perfect users: %.6f', perfectUsers));
                disp(sprintf('celda: %s',char(strcat(char('A'+5*(size(DISCARD_FACTORS,2)+1)+currTol),string(1+currDisc+5*currSizeObs)))));
                xlwrite(filename,perfectUsers,char(strcat('TRAIN=', string(TRAINING))),char(strcat(strcat('A',char('A'+(size(DISCARD_FACTORS,2)+1)+currTol)),string(1+currDisc+5*currSizeObs))));
                disp(sprintf('Attempts to decrypt on average: %.6f', avgAttemptsToDecrypt));
                xlwrite(filename,avgAttemptsToDecrypt,char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+3*(size(DISCARD_FACTORS,2)+1)+currTol),string(1+currDisc+5*currSizeObs))));
                disp(sprintf('Average of probability to not decrypt: %.4f',avgProbNoDecrypt) );
                xlwrite(filename,avgProbNoDecrypt,char(strcat('TRAIN=', string(TRAINING))),char(strcat(char('A'+4*(size(DISCARD_FACTORS,2)+1)+currTol),string(1+currDisc+5*currSizeObs))));
                disp('');
            end
        end
    end
end
        