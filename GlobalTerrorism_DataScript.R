
# Celem projektu jest:  Analiza jak koształtuje się ilość ataków terrorystycznych w 
# okresie wakacyjnym ( czerwiec, lipiec,sierpień) w latach 1970-2017.


# Sprawdzimy też jaka jest zależność miedzy ilością ataków, a liczbą ofiar i rannych.

# Plan projektu:
# 1.Ładowanie danych
# 2.Przygotowanie danych
# 3.Czyszczenie danych.
# 4. Analiza danych z użyciem ggplot2
####  - Analiza ataków w sezonie turystycznym
#5.  Regresja liniowa.
#### Zastosujemy 3 modele regresji, by porównać 3 zbiory danych i jak ich linie się kształtują na wykresie.
#6. Drzewa decyzyjne
#### Wykorzystamy drzewo decyzyjne by sprawdzić czy podczas ataku były ofiary śmiertelne w sytuacji gdy był też zakładnik.
#### Zobaczymy również jak zastosowanie/usunięcie filtrów zmienia wyniki analizy.
#7. K-średnich (K-means)




# Ładujemy dane z dysku z oryginalną tabelą - "GlobalTerrorism".
#Nazwa GlobalTerrorism w skrypcie będzie przypisana wyłącznie do pierwotnego źródła danych.

library(tidyverse)
library(readr)
# W moim przypadku dane źródłowe znajdowały się w poniższym folderze (ten element należy podmienić)

# Wpisz poniżej swoją własną ŚCIEŻKĘ DO PLIKU, BEZ TEGO DALSZA ANALIZA NIE ZADZIAŁA:  :|
GlobalTerrorism <- read_csv("C:/Users/Damian/OneDrive/DataEngineering/GlobalTerrorism.csv")

#W przypadku ewentualnych problemów, możemy użyć poniższego skryptu.
problems(GlobalTerrorism)

##################################################################################################################


# Plik zawiera 135 zmiennych i 181691 obserwacji
head(GlobalTerrorism)


######################### Przygotowanie danych ######################################


# W pierwszej części sprawdzamy jakie mamy nazwy kolumn w oraz jakie są to typy danych.
str(GlobalTerrorism)
spec(GlobalTerrorism)
colnames(GlobalTerrorism)



# Dodajemy nową kolumnę do tabeli Global Terrorism o nazwie PK (Primary Key), posłuży nam jako unikalna wartość
#numeryczna dla każdej zmiennej. Obecnie mamy taką kolumnę jak eventid, ale jest ona nie czytelna.

PK<-c(1:181691)
#print(PK)
gtcolA<-cbind(GlobalTerrorism,PK)
length(gtcolA$PK)


# Usuwamy kolumny, które nie będą przydatne w dalszej pracy ponieważ zawieraja zbyt dokładne dane i mogą 
# spowalniać dalsze analizy.Ze względu na dużą liczbę kolumn (135), łatwiej będzie wyciągnąć tylko te 
# kolumny nad którymi będziemy pracować niż usuwać z głównego zbioru różne zmienne.


# Ze wszystkich 135 kolumn wybieramy te które posłuża do dalszej analizy (24 kolumny) i przypisujemy je do obiektu gtcol

# -----------------------------------------------------------

#Obiekt gtcol będzie naszym bazowym źródłem danych.!!!!!!!!!!

# -----------------------------------------------------------


gtcol<-gtcolA%>% select (c( "PK","iyear","imonth","iday","country_txt",
                                    "region_txt","region_txt","city","latitude","longitude",
                                    "success","suicide","attacktype1_txt","targtype1_txt","targsubtype1_txt",
                                    "natlty1_txt","gname","claimmode_txt","weaptype1_txt","weapsubtype1_txt",
                                    "nkill","nwound","ransom","ishostkid","hostkidoutcome_txt"))
# Ponownie sprawdzamy dane.
length(gtcol)
head(gtcol)
str(gtcol)

# Zamieniamy nazwy kolumn, by były bardziej intuicyjne i zaczynały się z dużej litery.

colnames(gtcol)
gtcol<- gtcol %>% rename(Year=iyear,
                        Month=imonth,
                        Day=iday,
                        Country = country_txt,
                        Region = region_txt,
                        City = city,
                        Latitude = latitude,
                        Longitude = longitude,
                        Success = success,
                        Suicide = suicide,
                        Attack_type = attacktype1_txt,
                        Target = targtype1_txt,
                        Subtarget = targsubtype1_txt,
                        Nationality = natlty1_txt,
                        Group = gname,
                        Claim_request = claimmode_txt,
                        Weapon_type = weaptype1_txt,
                        Weapon_subtype = weapsubtype1_txt,
                        Killed = nkill,
                        Wounded = nwound,
                        Ransom = ransom,
                        Kidnapp_outcome = hostkidoutcome_txt)

# Sprawdzamy,na kilka sposobów czy nazwy kolumn zostały poprawnie przypisane i czy ewentualnie czegoś nie popsuliśmy.
colnames(gtcol)
head(gtcol)
tail(gtcol)


################ Czyszczenie danych ######################################



# Sprawdzamy czy w kolumnach są puste wartości i ile ich jest.

sapply(gtcol, function(x) sum(is.na(x)))

# Wybieramy kolumny z pustymi wartościami i wstawiamy w ich miejsce domyślne wartości. 
# Jesli będzie to możliwe, chcemy uniknąć kasowania całych obserwacji. 

gtcol <- gtcol %>%
  mutate(Subtarget = coalesce(Subtarget, "Unknown"),
         Nationality = coalesce(Nationality, "Unknown"),
         Claim_request = coalesce(Claim_request, "Unknown"),
         City = coalesce(City, "Unknown"),
         Latitude = coalesce(Latitude, 0),
         Longitude = coalesce(Longitude, 0),
         Weapon_subtype = coalesce(Weapon_subtype = "Unknown"),
         Killed = coalesce(Killed, 0),
         Wounded = coalesce(Wounded,0),
         Ransom = coalesce(Ransom,0),
         ishostkid = coalesce(ishostkid,0),
         Kidnapp_outcome = coalesce(Kidnapp_outcome,"Unknown"))

# Ponownie weryfikujemy dane czy pozbyliśmy się NA i czy nie ma wartości NULL
sapply(gtcol, function(x) sum(is.na(x)))
sapply(gtcol, function(x) sum(is.null(x)))


# W kolumnie Ransom mamy błędną wartość -9. 
# Domyślnie chcemy mieć opcje binarną ( 0,1), więc musimy błędna wartość podmienić. 
distinct(gtcol,Ransom)
gtcol$Ransom[gtcol$Ransom == -9] <- 0
distinct(gtcol,Ransom)

# Analogicznie w kolumnie ishostkid. Również błędną wartosć zamieniamy na 0.
distinct(gtcol,ishostkid)
gtcol$ishostkid[gtcol$ishostkid == -9] <- 0
distinct(gtcol,ishostkid)


# W kolumnie Target możemy uprościć jeden z celi ataków i połączyc 2 typy jako jeden - "Government"

gtcol$Target[gtcol$Target == "Government (Diplomatic)"] <- "Government"
gtcol$Target[gtcol$Target == "Government (General)"] <- "Government"
distinct(gtcol,Target)
names(gtcol)

distinct(gtcol,Claim_request)



################################ ANALIZA DANYCH ############################################



# Na początek sprawdzamy ile odbyło się różnych typów ataków terrorystycznych w latach 1970-2017.
# Wyniki możemy pokazać za pomocą zbioru danych oraz na wykresie słupkowym ( od najmniejszej do największej wartości).

gt_Target<-gtcol%>% distinct(Target,PK)%>%
                    group_by(Target)%>%
                    summarise("Attacks"=n())
gt_Target<-gt_Target%>%
                    arrange(desc(Attacks))
print(gt_Target, n=21)

gtcol%>%ggplot(aes(fct_infreq(Target)))+
        geom_bar(width=0.7,fill="#97B3C6")+
        coord_flip()+
        theme_bw()+
        labs( x = "Target group",
        y = "Number of attacks")+
        ggtitle("Number of attacks on various Targets")


#Sprawdzamy ile osób zgineło/zostało rannych w różnych typach ataków terrorystycznych oraz ile było takich ataków
# w latach 1970-2017.

gt_Attack_type <- gtcol %>% 
                      group_by(Attack_type) %>%
                      summarise("Attacks_count"=n(),
                      People_killed=sum(Killed),
                      People_wounded = sum(Wounded))
print(gt_Attack_type)

gtcol%>%ggplot(aes(x=Attack_type,y=Killed))+
                geom_col(fill="#60ACA8")+
                coord_flip()+
                theme_bw()+
                ggtitle("Number of casulties in various attack types (1970-2017)")

# Możemy też to wyrazić w ujęciu procentowym (%)

gt_Attack_percentage <- gtcol %>% 
                        group_by(Attack_type) %>%
                        summarise("Attacks_count"=n()/181691,
                            People_killed=sum(Killed)/411868,
                            People_wounded = sum(Wounded)/523869)
print(gt_Attack_percentage)



# Przyjrzyjmy się atakom terrorystycznych wg poszczególnych krajów w okresie (1970-2017).
# Obliczamy ile ataków odbyło się w poszczególnym kraju, ile osób w sumie zgineło, a ile zostało rannych.

gt_country2<-gtcol%>% distinct(Year,Country,PK,Killed,Wounded)%>%
                      group_by(Year,Country)%>%
                      summarise("Attack_count"=n(),
                                Victims_killed=sum(Killed),
                                Victims_wounded=sum(Wounded))
gt_country2<-gt_country2%>%arrange(desc(Attack_count))
print(gt_country2, n=100)


# Zawężamy analizę,by przyjrzeć się atakom w kilku popularnych miejscach turystycznych.

# Na wykresach można wyodrębnić kraje, gdzie ataki nie nasilały się. 
# Mamy też kraje (Egipt, Turcja) gdzie liczba znacząco waha się w różnych okresach, powodując odstępstwa od normy.
# Te odstępstwa mogą mieć wpływ na prawidłową ocenę, poniewaz można je traktować jako odstępstwa od normy.

gt_country3<-filter(gt_country2,Country %in% c("Turkey","Egypt","Tunisia","Israel","Mexico"))
print(gt_country3,n=192)

ggplot(data = gt_country3,aes(x=Year,y=Attack_count,colour=Country)) +
                        geom_line(aes(colour=Country))+
                        geom_point(alpha=0.3)+
                        theme_bw()+
                        facet_grid(Country~.)+
                        #theme_minimal()+
                        labs(title= "Terrorist attacks in popular holiday destinations")+
                        xlab("Years 1970-2017")+
                        ylab("Number of attacks")


# Następnie obserwujemy jak liczba ataków kształtuje się na tle ilości zabitych i rannych.

# Na tym polu również zauważymy, że w krajach z większą liczba ataków, zarówno ilość ofiar i rannych się zwiększa.
# Szczególnie widać to w ostatnich latach w Egipcie i Turcji. 
# Na całościową analize w przypadku Izraela również będzie miał wpływ dużej ilości zgonów na poczatku XXI wieku. 

ggplot(data = gt_country3,aes(x=Year))+
      geom_line(aes(y=Attack_count),color="green")+
      geom_line(aes(y=Victims_killed),color="red")+
      geom_line(aes(y=Victims_wounded),color="black")+
      theme_bw()+
      facet_grid(Country~.)+
      #theme_minimal()+
      labs(title= "Terrorist attacks vs. deaths vs. wounded in popular holiday destinations")+
      xlab("Years 1970-2017")

# Zbiorcza ilość ataków, ofiar i rannych dla analizowanych krajów w liczbach.

gt_country4<-gtcol%>% distinct(Country,PK,Killed,Wounded)%>%
  group_by(Country)%>%
  summarise("Attack_count"=n(),
            Victims_killed=sum(Killed),
            Victims_wounded=sum(Wounded))
#gt_country4<-gt_country4%>%arrange(desc(Attack_count))
gt_country5<-filter(gt_country4,Country %in% c("Turkey","Egypt","Tunisia","Israel","Mexico"))
print(gt_country5)




gt_b<-gtcol%>% 
  distinct(Year,Country,Month,PK,Attack_type,Target,Killed,Wounded,
           Ransom,Weapon_type,Weapon_subtype,ishostkid,Success)%>%
  filter(Country=="Mexico" 
         | Country=="Tunisia" | Country=="Israel")%>%
  group_by(Year,Month,Country,Target,Attack_type,
           Weapon_type,Weapon_subtype,ishostkid,Ransom,Success)%>%
  summarise("Attack_count"=n(),
            Victims_killed=sum(Killed),
            Victims_wounded=sum(Wounded))

# #Boxplot - Możemy również porównać dane dla 3 krajów.

# Pokazuje on że w większości przeprowadzonych ataków, gineły pojedyncze osoby.
# Świadczy o tym odległość miedzy medianą a 3cim kwartylem. Obserwujemy wąskie outliery, gdyż cześć wyników została 
# zaklasyfikowana do wartości odstających - we wszystkich krajach dochodziło do pojedynczych ataków,
# w ciagu roku które były bardzije śmiertelne.
# Rozstęp ćwiartkowy również będzie nie wielki.

gt_b
gt_b %>%
  ggplot(aes(x=Country,y=Victims_killed))+
  geom_jitter()+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  ylab("Victims killed")+
  ggtitle("Number of kills comparison using Boxplot for 3 countries")



######################## Ataki w sezonie turystycznym  ########################



# Wiemy już jak kształtowały sie ataki popularnych krajach, teraz chcemy dalej zawęzić dane i przyjrzeć sie atakom
# w sezonie tzw. turystycznym czyli miesiącach: czerwiec, lipiec, sierpień.

# Na początek spójrzmy ogólnie jak ataki kształtują sie w tym okresie dla wszstkich krajów.

gt_s<-gtcol%>% distinct(Year,Country,Target,Month,PK,Killed,Wounded)%>%
              filter(Month %in% c(6,7,8))%>%
              group_by(Year,Month,Country,Target)%>%
              summarise("Attack_count"=n(),
                        Victims_killed=sum(Killed),
                        Victims_wounded=sum(Wounded))
gt_s<-gt_s%>%arrange(desc(Attack_count))
print(gt_s, n=100)


# Zmniejszamy zakres danych do 5 popularnych turystycznie krajów, 2 celów ataków oraz miesięcy wakacyjnych. 

gt_s2<-filter(gt_s,Country %in% c("Turkey","Egypt","Tunisia","Israel","Mexico"),
              Target %in% c("Tourism","Private Citizens & Property"),
              Month %in% c(6,7,8))
#print(gt_country3,n=50)
print(gt_s2)

# Poniższe wykresy zawężają analizę. 
# Obrazują jak ataki terrorystyczne wymierzone w docelowo w ludność cywilną i turystów kształtowały się 
# w okresie "wakacyjnym" oraz ile osób zgineło lub zostało rannych.

# Ponownie poniższe wykresy dla krajów: Meksyk i Tunezja zachowują pewną stabilność. 
# Natomiast widać, że w Izraelu oraz Turcji pojawiają się duże wahania.

ggplot(data = gt_s2,aes(x=Year))+
  geom_line(aes(y=Attack_count),color="green")+
  geom_line(aes(y=Victims_killed),color="red")+
  geom_line(aes(y=Victims_wounded),color="black")+
  theme_bw()+
  facet_grid(Country~.)+
  #theme_minimal()+
  labs(title= "Number of attacks vs deaths vs wounded in holiday seasson")+
  xlab("Years 1970-2017")


# Boxplot
# Ponownie używamy Boxplot, ale tym razy wybieramy 2 ( Meksyk,Tunezja) kraje, które na poprzednim wykresie 
# wykazywały stabilną korelacje miedzy 3 liniami.Dodajemy do tego Izrael, by porównać kraj z większymi wahaniami. 

# Z poprzedniego wykresu liniowego możemy zapamietać nagły/skokowy wzrost ataków w Izraelu w jednym roku (linia zielona).
# Możemy to również zauważyć, po kształcie Boxplot dla tego kraju, który jest znacznie węższy od Meksyku. 
# Dzięki geom_jitter, również widzimy, że dużo zdarzeń, znajduje się przy lewej krawędzi figury (Izrael).
# Duża śmiertelność za to pojawia się w Meksyku, co widać po szerokości miedzy medianą, a 3 kwartylem. W tym przypadku, również widać
# stabilność poprzedniego wykresu, gdyż praktycznie wszystkie punkty na Boxplot sa w granicach "wąsów".


gt_s2 %>%
  filter(Country=="Mexico" 
         | Country=="Tunisia" | Country=="Israel") %>%
  ggplot(aes(x=Country,y=Victims_killed))+
  geom_jitter()+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  ylab("Victims killed")
  #scale_y_continuous(limits=c(0,80))



################## Regresja liniowa ############################################


gt_country2<-gtcol%>% distinct(Year,Country,PK,Killed,Wounded)%>%
                      group_by(Year,Country)%>%
                      summarise("Attack_count"=n(),
                      Victims_killed=sum(Killed),
                      Victims_wounded=sum(Wounded))
gt_country2<-gt_country2%>%arrange(desc(Attack_count))
print(gt_country2, n=100)


# Budujemy wzór regresji liniowej.
# Chcemy sprawdzić czy wraz ze wzrostem liczby ataków w ujęciu rocznym, wzrośnie też liczba ofiar.

gt_country2%>%
    lm(Victims_killed ~ Attack_count,data = .) %>%
    summary()

# Użyliśmy regresji liniowej dla wszystkich krajów z tabeli, czyli uwzględniliśmy również te kraje
# gdzie było zarówno dużo ataków jak i wysoka śmiertelność. W wyniku zapytania otrzymaliśmy R2= 0.7338.
# Wysoka wartość R2 w tym przypadku oznacza, że jest 73% prawdopodobieństwo że wzrost ataków zwiększy liczbę ofiar.


# 2ga opcja by stworzyć taką samą regresje liniową, ale zapiszemy ją w obiekcie.
 # łatwiej też można ją przegladać.

mod<-lm(Victims_killed ~ Attack_count,data=gt_country2)
mod
summary(mod)
attributes(mod)
mod$residuals
# Mozemy tez zrobić wizualizację za pomoca Histogramu oraz dodać linię.
hist(mod$residuals)
abline(mod)


#### Regresja dla popularnych krajów gdzie ataki odbyły sie w okresie turystycznym. ################


# Zgodnie założeniem analizy, zmniejszymy dane do 5 krajów:
# obiekt mod2 = (Turkey,Egypt,Tunisia,Israel,Mexico) i zawęzimy występowanie ataków do okresu wakacyjnego oraz
# 2 celów ataków: Turyści i Ludność cywilna.

# Widać znaczącą zmianę dla współczynników Residuals, który przybliżył się do 0.
# R-squared[r2] = 0.02229, a linia (slope) znacznie przechyla sie w prawą stronę w stosunku do 
# pierwszej analizy z użyciem wszystkich krajóW, gdyż wyeliminowaliśmy kraje z wysoką śmiertelnością. 
# Dzięki czemu punktowy wzrost osi X nie wpływa nie proporcjonalnie wysokim wzrostem y.

mod2<-lm(Victims_killed~Attack_count,data=gt_s2)
mod2
summary(mod2)
hist(mod2$residuals)
abline(mod2)


# Na koniec dodamy jeszcze 3 analize mod3= Meksyk, Tunezja. + okres wakacyjny + Cel: Tylkoturyści i ludność cywilna. 

# Celowo pominiemy kraje, gdzie w ubiegłych latach dochodziło do nagłej zwiększonej ilości ataków. 
# Takie zawężenie wyników jednak nie zmniejszyło kątu nachylenia, widać to po samym kształcie linii oraz porównując
# wartość slope mod2 = 0.19   mod3= 0.69

# W Wyniku ostatniej analizy nasz R2= 0.014.
# Porównując zarówno wartości R2 jak i kształt linii (abline) dla mod, mod2 i mod3 widzimy, że wyniki krajów, z wysoką
# liczbą ataków oraz wysoką śmiertelnością maja wyraźny wpływ na kształt regresji liniowej.


gt_s3<-filter(gt_s,Country %in% c("Tunisia","Mexico"),
              Target %in% c("Tourism","Private Citizens & Property"),
              Month %in% c(6,7,8))
print(gt_s3)

# Zawężamy analize do 2 krajów: Tunezja i Meksyk, gdzie wykres liniowy był najbardziej stabilny w okresie wakacyjnym.

mod3<-lm(Victims_killed~Attack_count,data=gt_s3)
mod3
summary(mod3)
hist(mod2$residuals)
abline(mod3)



################# Drzewa decyzyjne ##########################################

# Skupiamy się dalej na 5 krajach oraz atakach w okresie wakacyjnym na ludność cywilną oraz turystów.
# Będzie nas również interesować czy w wyniku ataku brani byli zakładnicy, czy był okup oraz czy ktoś zginął.


# ładujmey pakiety - rpart i rpart.plot
# Sam rpart wyświetli nam tylko dane, a rpart.plot pozwoli to lepiej zwizualizować za pomoca drzewa decyzyjnego.

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#install.packages("party")
#library(party)


#Tworzymy model danych, do którego możemy ewentualnie dodać kolumny lub wprowadzać fitry zmiennych.
gt_Tree<-gtcol%>% 
          distinct(Year,Country,Month,PK,Attack_type,Target,Killed,Wounded,
                   Ransom,Weapon_type,Weapon_subtype,ishostkid,Success)%>%
          group_by(Year,Month,Country,Target,Attack_type,
                   Weapon_type,Weapon_subtype,ishostkid,Ransom,Success)%>%
          summarise("Attack_count"=n(),
                    Victims_killed=sum(Killed),
                    Victims_wounded=sum(Wounded))

#gt_Tree<-gt_Turkey%>%arrange(desc(Attack_count))
gt_Tree2<-filter(gt_Tree,Country %in% c("Turkey","Mexico","Tunisia","Egypt","Israel"),
                    Target %in% c("Tourism","Private Citizens & Property"),
                    #Attack_type %in% c("Armed Assault","Assasination","Bombing/Explosion"),
                    Month %in% c(6,7,8))
head(gt_Tree2)



# Ustalamy próbę na 70% i 30%
set.seed(1234)
indexSet<-sample(2,nrow(gt_Tree2), replace=T, prob = c(0.7,0.3))
train<- gt_Tree2[indexSet==1,]
test<- gt_Tree2[indexSet==2,]

# Budujemy drzewo decyzyjne na podstawie zbioru Train.

# Sprawdzamy ile ataków zakończyło się "sukcesem", przy założeniu że: 
# mamy podany kraj,czy był wzięty zakładnik, czy był okup oraz czy były ofiary śmiertelne.


# Możemy te dane zmieniać w dowolny sposób na podstawie parametrów z modelu gt_Tree2.
# Z definicji skupiamy sie danych zawartych w filtrze, ale też można je wyłączyć by zwiększyć 
# ilość zmiennych oraz by wyniki były ciekawsze.

# Zrobimy 2 drzewa decyzyjne: 
# W pierwszym sposobie zdefiniujemy parametry ~ "po prawej strony tyldy".
# W drugim podejściu zastąpimy parametry z prawej strony ~ znakiem . 

# Poniższe drzewo skupia sie na atakach w okresie wakacyjny, 
# ale usuwając ten filtr zwiększymy zbiór w naszym drzewie o ...347%   

Class_tree<-rpart(Success ~ Country + ishostkid + Ransom + Victims_killed,
                  data=train,
                  method="poisson")

# Wybraliśmy method=poisson, ale anova/class również się sprawdzają. 
# Możemy sprawdzić w liczbach jak kształtuje sie dane.
Class_tree

#Wybarliśmy type=4, by lepiej zobrazować drzewo.
rpart.plot(Class_tree,type=1,fallen.leaves = FALSE,faclen = 0,cex=0.9) + 
           title(" How attacks can be succesful in case when hostage is taken 
                 and potentially killed?")
?rpart.plot

##### Drugi sposób tym razem, zmnieniamy oznaczenie z prawej strony i parametr fallen.trees.
# Ten sposób wygląda bardziej czytelnie.

Class_tree2<-rpart(Success ~ .,
                  data=train,
                  method="poisson")

rpart.plot(Class_tree2,type=1,fallen.leaves = TRUE,faclen = 0,cex=0.9) + 
  title(" How attacks can be succesful in case when hostage is taken 
                 and potentially killed?")

############################## K-means ########################################

#install.packages("ClusterR")
#install.packages("cluster")

library(ClusterR)
library(cluster)

# Wyciągamy kolumny liczbowe z naszej głównej bazy danych.
gtk3<-gtcol%>%select(c("Ransom","Killed","Wounded"))
gtk3

# Tworzymy model K-means i użyjemy 10 losowo wybranych setów. 
set.seed(240)
kmeans.re<-kmeans(gtk3,centers=3,nstart=10)

?kmeans

# Sprawdzamy wynik. Otrzymujemy 3 klastry, o wielkościach:4, 902 , 180785.
# Suma kwadratów = 78.8%
kmeans.re

kmeans.re$cluster

#Tworzymy confusiom matrix
cm<-table(gtcol$Attack_type,kmeans.re$cluster)
cm

kmeans.re$cluster

plot(gtk3[c("Killed","Wounded")],
     col=kmeans.re$cluster)

#Szukamy środka klastrów
# Przez to, że niektóre punkty są mocno odchylone od pozostałych, sporo punktów znajduje sie po lewej stronie. Gdyż w większości ataków, 
# gineło kilka osób. Musimy pamiętać, że w naszej analizie całościowej, również były pojedyncze ataki, gdzie gineło wiecej niż 1000 osób np.
# atak na World Trade Center. Takie pojedyncze ataki sprawiają, że wizualizacja jest mało czytelna.

kmeans.re$centers[,c("Killed","Wounded")]
points(kmeans.re$centers[,c("Killed","Wounded")],
       col=1:3,pch=8,cex=3)                  

 
# Budujemy główną wizualizację, która grupuje dane wg klastrów.
y_kmeans <- kmeans.re$cluster
clusplot(gtk3[, c("Killed", "Wounded")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster Attacks"),
         xlab = 'Killed',
         ylab = 'Wounded')


################################################################################

# W powyższym projekcie stosowaliśmy różne metody do analizy danych. W każdym przypadku, gdy w analizie brały udział kraje,z wysoką liczbą
#ataków, lub śmiertelności to miało to końcowy wynik analizy. Nawet analizując kraje z mniejsza śmiertelnościa, a właczając do nich kraje z
# pojedyncza liczbą nagłych zwiększonych ataków, to w takim przypadku również miało to wpływ na wynik końcowy.
