# elisa
Prolog Eliza - psychotherapist chatbot

## Program description
Eliza is a computer application that speaks with a human in natural language. The speech is about his troubles, feelings and problems. Eliza act as some kind of psychotherapist. Because of the complexity of Czech language, the speech between Eliza and chatbot is in English.

The whole app was developed in Prolog. SWI-Prolog 7.2.2 (swipl) is recommended for correct running. This group of commands will run the Eliza:

~~~
$ swipl
?− consult ("elisa.pl").
?− elisa.
~~~
  
After startup, Eliza says hi to the user and gives him space for a reply.... 

~~~
Hello, I ’m Elisa, how can I help you?
> [INPUT]
~~~

The program could be terminated by saying goodbye to Eliza....

~~~
> Bye, have a nice day.
Goodbye. My secretary will send you a bill.
?− halt.
~~~
