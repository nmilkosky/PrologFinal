#Prolog Project
https://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html

Demo queries
1 (Constructor): parse([public,class,test,'{',public,test,'(',')','{','}','}']).
2 (Method w/ Var Dec): parse([public,class,test,'{',public,int,method,'(',')','{',int,x,=,3,';','}','}']).
3 (Method w/ Params): parse([public,class,test,'{',public,int,method,'(',int,var1,',',"String",var2,')','{','}','}']).
4 (Main Method): parse([public,class,test,'{',public,static,void,method,'(',"String",[],args,')','{','}','}']).
5 (Method w/ Return): parse([public,class,test,'{',public,int,method,'(',int,[],nums,')','{',return,x,';','}','}']).
5 (Expressions): once(expression([x,==,y],[])).
6 (While Loop): once(whileStatement([while,'(',x,==,y,')',y,++,';'],[])).
7 (2 missing modifier): parse([public,class,test,'{',int,method,'(',')','{',int,x,=,3,';','}','}']).
8 (3 missing comma): parse([public,class,test,'{',public,int,method,'(',int,var1,"String",var2,')','{','}','}']).