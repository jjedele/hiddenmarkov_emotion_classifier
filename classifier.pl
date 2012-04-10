/* vim: set filetype=prolog: */

% Emotions = [anger, disgust, disdain, surprise, fear].

/* hidden markov model */
% transition_probability(+ToEmotion, +FromEmotion, -Probability).
transition_probability(anger, start, 0.2).
transition_probability(disgust, start, 0.2).
transition_probability(disdain, start, 0.2).
transition_probability(surprise, start, 0.2).
transition_probability(fear, start, 0.2).
transition_probability(anger, anger, 0.63).
transition_probability(disgust, anger, 0.13).
transition_probability(disdain, anger, 0.05).
transition_probability(surprise, anger, 0.11).
transition_probability(fear, anger, 0.08).
transition_probability(anger, disgust, 0.15).
transition_probability(disgust, disgust, 0.58).
transition_probability(disdain, disgust, 0.16).
transition_probability(surprise, disgust, 0.04).
transition_probability(fear, disgust, 0.07).
transition_probability(anger, disdain, 0.09).
transition_probability(fear, disdain, 0.03).
transition_probability(disgust, disdain, 0.21).
transition_probability(disdain, disdain, 0.58).
transition_probability(surprise, disdain, 0.09).
transition_probability(anger, surprise, 0.06).
transition_probability(fear, surprise, 0.18).
transition_probability(disgust, surprise, 0.23).
transition_probability(disdain, surprise, 0.14).
transition_probability(surprise, surprise, 0.39).
transition_probability(anger, fear, 0.09).
transition_probability(fear, fear, 0.56).
transition_probability(disgust, fear, 0.14).
transition_probability(disdain, fear, 0.06).
transition_probability(surprise, fear, 0.15).
% emission_probability(+FeatureVector, +Emotion, -Probability).
emission_probability(0, anger, 0.15).
emission_probability(1, anger, 0.09).
emission_probability(2, anger, 0.06).
emission_probability(3, anger, 0.07).
emission_probability(4, anger, 0.45).
emission_probability(5, anger, 0.04).
emission_probability(6, anger, 0.05).
emission_probability(7, anger, 0.09).
emission_probability(0, fear, 0.06).
emission_probability(1, fear, 0.06).
emission_probability(2, fear, 0.07).
emission_probability(3, fear, 0.09).
emission_probability(4, fear, 0.05).
emission_probability(5, fear, 0.43).
emission_probability(6, fear, 0.04).
emission_probability(7, fear, 0.20).
emission_probability(0, disgust, 0.16).
emission_probability(1, disgust, 0.31).
emission_probability(2, disgust, 0.08).
emission_probability(3, disgust, 0.11).
emission_probability(4, disgust, 0.09).
emission_probability(5, disgust, 0.09).
emission_probability(6, disgust, 0.10).
emission_probability(7, disgust, 0.06).
emission_probability(0, disdain, 0.17).
emission_probability(1, disdain, 0.16).
emission_probability(2, disdain, 0.31).
emission_probability(3, disdain, 0.28).
emission_probability(4, disdain, 0.02).
emission_probability(5, disdain, 0.03).
emission_probability(6, disdain, 0.02).
emission_probability(7, disdain, 0.01).
emission_probability(0, surprise, 0.01).
emission_probability(1, surprise, 0.06).
emission_probability(2, surprise, 0.04).
emission_probability(3, surprise, 0.10).
emission_probability(4, surprise, 0.10).
emission_probability(5, surprise, 0.11).
emission_probability(6, surprise, 0.12).
emission_probability(7, surprise, 0.46).

/* program */
% viterbi(+Emotions, +Observations, -ResultSequence).
viterbi(Emotions, Observations, Result) :-
	is_list(Observations),
	length(Observations, OL),
	OL >= 1,
	initial_probabilities(Emotions, InitProbs), % initialize the 0th column of matrix
	process_observations(Observations, 1, Emotions, InitProbs, Result) % start recursive processing of observations
	.

initial_probabilities([], []).

initial_probabilities([EmoH|EmoT], InitialProbs) :-
	initial_probabilities(EmoT, IntermediateProbs), % descend over emotion vector
	transition_probability(EmoH, start, CurProb), % add the transition probability from <<start>> to current emotion to vector
	InitialProbs = .(EmoH-CurProb, IntermediateProbs)
	.

process_observations([], Count,  _, LastColumn, Result) :-
	true, % don't know why, but it only works like this
	LastCount is Count-1,
	column_max(LastColumn, Max, ArgMax), % find max probability in last column,
	viterbi_path(ArgMax, LastCount, Result)
	.

process_observations([OH|OT], Count, Emotions, LastColumn, Result) :-
	process_emotions(OH, Emotions, Count, LastColumn, Column),
	NewCount is Count+1,
	process_observations(OT, NewCount, Emotions, Column, Result)
	.

process_emotions(O, [], _, _, []).

process_emotions(O, [EH|ET], Count, FormerProbs, UpdatedMatrixColumn) :-
	process_emotions(O, ET, Count, FormerProbs, MatrixColumn),
	best_predecessor(FormerProbs, EH, O, Max, ArgMax),
	asserta(matrix_path(Count, EH, ArgMax)),
	UpdatedMatrixColumn = .(ArgMax-Max, MatrixColumn)
	.

best_predecessor([], _, _, -1, na).

best_predecessor(FormerProbs, CurrentEmotion, Observation, Max, ArgMax) :-
	is_list(FormerProbs), % check that FormerProbs is a list
	length(FormerProbs, LL),
	LL >= 1,
	pairs_keys(FormerProbs, [FormerEmotion|FormerKeysT]), % FormerProbs is a list of key-value pairs - seperate keys from values for further processing
	pairs_values(FormerProbs, [FormerProb|FormerValsT]),
	pairs_keys_values(FormerProbsT, FormerKeysT, FormerValsT), % and put the rests together again for recursion
	best_predecessor(FormerProbsT, CurrentEmotion, Observation, OtherMax, OtherArgMax), % descend over complete former matrix column
	transition_probability(CurrentEmotion, FormerEmotion, TransProb), % calculate the complete probability for the current cell 
	emission_probability(Observation, CurrentEmotion, EmiProb),
	CurrentProb is FormerProb*TransProb*EmiProb,
	!,
	(CurrentProb >= OtherMax -> Max is CurrentProb, ArgMax = FormerEmotion ; Max is OtherMax, ArgMax = OtherArgMax) % manage max
	.

column_max([], -1, na).

column_max(Column, Max, ArgMax) :-
	is_list(Column),
	length(Column, LL),
	LL > 0,
	pairs_keys(Column, [KeyH|KeyT]),
	pairs_values(Column, [ValH|ValT]),
	pairs_keys_values(ColumnT, KeyT, ValT),
	column_max(ColumnT, OtherMax, OtherArgMax),
	(ValH >= OtherMax -> Max is ValH, ArgMax = KeyH ; Max is OtherMax, ArgMax = OtherArgMax)
	.

viterbi_path(_, 0, []).

viterbi_path(Observation, PathLength, Path) :-
	HeadLength is PathLength-1,
	matrix_path(PathLength, Observation, LastObservation),
	viterbi_path(LastObservation, HeadLength, HeadPath),
	%!,
	append(HeadPath, [LastObservation], Path)
	.

% viterbi([anger, disgust, disdain, surprise, fear], [0, 0, 0], X).