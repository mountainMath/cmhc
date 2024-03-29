#' Internal function to generate hex sticker
#' @keywords internal
generate_cmhc_hex_sticker <- function (){
  library(tidyverse)
  library(cmhc)
  ggplot2::theme_set(ggplot2::theme_gray())
  inflation <- cansim::get_cansim_vector("v41690973") |>
    select(Date,CPI=val_norm) |>
    mutate(CPIchange=CPI/lag(CPI,order_by = Date,n=12)-1)

  pd1 <- get_cmhc("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods","48825") |>
    bind_rows(get_cmhc("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods","48825",filter=list("season"="April"))) |>
    mutate(Value=Value/100)
  pd2 <- get_cmhc("Rms","Average Rent Change","Bedroom Type","Historical Time Periods","48825") |>
    bind_rows(get_cmhc("Rms","Average Rent Change","Bedroom Type","Historical Time Periods","48825",filter=list("season"="April"))) |>
    mutate(Value=Value/100) |>
    left_join(inflation,by="Date") |>
    mutate(Value=Value-CPIchange)

  pd <- bind_rows(pd1,pd2) |>
    filter(`Bedroom Type`=="Total") %>%
    left_join(filter(.,!is.na(Value)) |>
                select(Series,Date) |>
                group_by(Series) |>
                mutate(diff=as.integer(Date-lag(Date,order_by=Date))),
              by=c("Date","Series")) |>
    group_by(Series) |>
    filter(!is.na(Value) | (!is.na(lead(diff,order_by = Date)) & lead(diff,order_by = Date)>366))


  read_white_image <- function(path){
    logo<-magick::image_read(path)
    img <- logo[[1]]
    img[1,,] <- as.raw(205)
    img[2,,] <- as.raw(133)
    img[3,,] <- as.raw(63)
    magick::image_read(img)
  }

  library(grid)
  i1 <- read_white_image(here::here("images/cityscape.png")) |> rasterGrob(interpolate=TRUE)
  i2 <- read_white_image(here::here("images/buildings.png")) |> rasterGrob(interpolate=TRUE)
  i3 <- read_white_image(here::here("images/skyline.png")) |> rasterGrob(interpolate=TRUE)

  crs <- "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs"

  ca_data <- cancensus::get_census("CA21",regions=list(C="01"),geo_format='sf') %>%
    sf::st_transform(crs)
  cities <- cancensus::get_census("CA21",regions=list(CMA=c("59933","47705","35535")),geo_format='sf') |>
    arrange(Population) |>
    sf::st_transform(crs) |>
    select() |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as_tibble()
  q <- ggplot2::ggplot(ca_data) +
    ggplot2::geom_sf(fill="#800000",size=0.01) +
    ggplot2::theme_void() +
    hexSticker::theme_transparent()
  bbox=sf::st_bbox(ca_data)
  p<-ggplot2::ggplot(pd %>% filter(!is.na(Value)),ggplot2::aes(x=Date,y=Value,color=Series)) +
    ggplot2::geom_point(aes(shape=Quality),size=0.8) +
    ggplot2::geom_line(size=0.3) +
    #ggplot2::scale_colour_brewer(palette="Dark2",guide="none") +
    ggplot2::scale_color_manual(values=c("Average Rent Change"="#f0c60a",
                                         "Vacancy Rate"="#f0430a"),
                                guide="none") +
    ggplot2::scale_shape_discrete(guide="none") +
    ggplot2::labs(x="",y="") +
    ggplot2::theme_void() +
    hexSticker::theme_transparent()

  xmin <- as.numeric(bbox$xmin)
  xwidth <- as.numeric(bbox$xmax-bbox$xmin)
  ymin <- as.numeric(bbox$ymin)
  ywidth <- as.numeric(bbox$ymax-bbox$ymin)

  size=0.1

  pp <- q +
    ggplot2::annotation_custom(i3, xmin=cities$X[1]-size*xwidth,xmax=cities$X[1]+size*xwidth,
                               ymin=cities$Y[1],ymax=cities$Y[1]+2*size*ywidth) +
    ggplot2::annotation_custom(i1, xmin=cities$X[2]-size*xwidth,xmax=cities$X[2]+size*xwidth,
                               ymin=cities$Y[2],ymax=cities$Y[2]+2*size*ywidth) +
    ggplot2::annotation_custom(i2, xmin=cities$X[3]-size*xwidth,xmax=cities$X[3]+size*xwidth,
                               ymin=cities$Y[3],ymax=cities$Y[3]+2*size*ywidth) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(p),
                               xmin=1.5*bbox$xmin,xmax=1.5*bbox$xmax,
                               ymin=bbox$ymin*0.8+bbox$ymax*0.2,ymax=bbox$ymax*1.1) +
    hexSticker::theme_transparent()

  hexSticker::sticker(pp, package="cmhc",
                      p_size=12, p_y=1.5,
                      s_x=1, s_y=0.78, s_width=1.5, s_height=1.5,
                      h_color="#FF0000",
                      h_fill="grey40",
                      p_color="white",
                      filename=here::here("logo.svg"))

  hexSticker::sticker(pp, package="cmhc",
                      p_size=22, p_y=1.65,
                      s_x=1, s_y=0.78, s_width=1.5, s_height=1.5,
                      h_color="#FF0000",
                      h_fill="grey40",
                      p_color="white",
                      filename=here::here("logo.png"))

}
