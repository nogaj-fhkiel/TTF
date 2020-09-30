#' Schwund Plot
#'
#' Zeigt die Anzahl der Exmatrikulierten nach Semester und Fachsemester
#'
#' @param basisdaten_roh data.table der Basisdaten zu den Studierenden enthält
#' @param Sem charakter_vektor der das Semester enthält
#' @param Abschl charakter_vektor der die Abschlussart enthält
#' @param Stg charakter_vektor der den Studiengang enthält
#'
#' @return ggplotobjekt
#' @export
#'
#' @examples
#' schwund_plot(basisdaten_roh,
#' c(
#'   "WiSe 2017/2018",
#'  "SoSe 2018",
#'   "WiSe 2018/2019",
#'   "SoSe 2019",
#'   "WiSe 2019/2020"
#' ),
#' "Bachelor",
#' "Betriebswirtschaft"
#' )
schwund_plot <- function(basisdaten_roh,Sem, Abschl, Stg) {
  Plotdata<-basisdaten_roh %>%
    .[Hörerstatus %in% c("Haupthörer", "Nebenhörer") &
        Semester %in% Sem & Abschluss == Abschl&
        Fach == Stg &
        !(Exmatrikulationsgrund %in% c("Beendig.Stud.nach Prüfung", NA))
      , sum(Summe), by = .(Semester, Anzahl.Fachsemester)] %>%
    .[Anzahl.Fachsemester <= 3, FS := "<=3"] %>%
    .[Anzahl.Fachsemester >= 4 &
        Anzahl.Fachsemester <= 6, FS := "4. - 6. FS"] %>%
    .[Anzahl.Fachsemester >= 7 &
        Anzahl.Fachsemester <= 9, FS := "7. - 9. FS"] %>%
    .[Anzahl.Fachsemester >= 10, FS := ">=10"] %>%
    .[, insgesamt := sum(V1), Semester] %>%
    .[, insgesamtfs := sum(V1), .(Semester, Anzahl.Fachsemester)] %>%
    .[, anteile := insgesamtfs / insgesamt, Semester] %>%
    .[, sum(anteile), .(Semester, FS)]
  Plotdata %>% ggplot(aes(x= factor(Semester,level=Sem),y=V1,fill=FS))+
    geom_col(position = "fill")+
    scale_fill_manual(values = as.character(c(colors_d1,colors_d2,colors_d3,colors_d4)))
}

