#Prolog Project
https://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html

Demo queries

1 (Constructor): parse([public,class,test,'{',public,test,'(',')','{','}','}']).

2 (Method w/ Var Dec): parse([public,class,test,'{',public,int,method,'(',')','{',int,x,=,3,';','}','}']).

3 (Method w/ Params): parse([public,class,test,'{',public,int,method,'(',int,var1,',',string,var2,')','{','}','}']).

4 (Main Method): parse([public,class,test,'{',public,static,void,method,'(',string,[],args,')','{','}','}']).

5 (Method w/ Return): parse([public,class,test,'{',public,int,method,'(',int,[],nums,')','{',return,x,';','}','}']).

6 (Methods 2): parse([public,class,test,'{',public,test,'(',')','{',int,x,=,3,';',int,y,=,5,';',if,'(',y,==,5,')',y,=,0,';','}','}']).

7 (Two Methods): parse([public,class,test,'{',public,test,'(',')','{','}',public,static,void,main,'(',')','{','}','}']).

8 (Two variables): parse([public,class,test,'{',public,test,'(',')','{',int,x,=,3,';',int,y,=,0,';','}','}']).

9 (Complex Statement): parse([public,class,test,'{',public,static,void,main,'(',string,[],args,')','{',int,x,=,3,';',int,y,=,5,';',if,'(',y,==,5,')',y,=,0,';','}','}']).

10 (1 missing id): parse([public,class,'{',public,test,'(',')','{','}','}']).

11 (2 missing modifier): parse([public,class,test,'{',int,method,'(',')','{',int,x,=,3,';','}','}']).

12 (3 missing comma): parse([public,class,test,'{',public,int,method,'(',int,var1,string,var2,')','{','}','}']).

13 (5 without close bracket): parse([public,class,test,'{',public,int,method,'(',int,'[',nums,')','{',return,x,';','}','}']).

14 (7 missing bracket): parse([public,class,test,'{',public,test,'(',')','{',int,x,=,3,';',int,y,=,0,';','}']).

15 (11 missing semicolon): parse([public,class,test,'{',public,static,void,main,'(',string,[],args,')','{',int,x,=,3,';',int,y,=,5,';',if,'(',y,==,5,')',y,=,0,'}','}']).