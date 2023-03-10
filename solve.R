library(tidyverse)
library(lubridate)

download.file(url = "https://hanukkah.bluebird.sh/5783/noahs-csv.zip", destfile = "noahs-csv.zip")
system("unzip -P 5777 noahs-csv.zip")

customers <- read_csv("noahs-customers.csv")
orders <- read_csv("noahs-orders.csv")
items <- read_csv("noahs-orders_items.csv")
products <- read_csv("noahs-products.csv")
orders_plus <- orders %>% inner_join(items) %>% inner_join(products) %>% inner_join(customers)

# Puzzle 1

letter_to_phonenumber <- function(letter) {
    letter_index <- which(letters == letter)
    phonenumber <- floor(((letter_index - 1) / 3) + 2)
    if (!letter %in% c("s", "v", "y", "z")) {
        return(as.character(phonenumber))
    } else {
        return(as.character(phonenumber - 1))
    }
}

name_to_phonenumber <- function(name) {
    surname <- (strsplit(name, " ") %>% unlist)[-1]
    name_lower <- str_to_lower(surname) %>% str_replace_all("[^a-z]", "")
    name_short <- str_sub(name_lower, end = 10)
    name_vector <- unlist(strsplit(name_short, ""))
    name_phonenumber_vector <- map_chr(name_vector, letter_to_phonenumber)
    return(name_phonenumber_vector %>% paste0(collapse = ""))
}

customers_with_phonenumbers <- customers %>% mutate(name_phone = map_chr(name, name_to_phonenumber)) %>% mutate(phoneraw = str_replace_all(phone, "-", ""))

investigator <- customers_with_phonenumbers %>% filter(phoneraw == name_phone)
investigator_phone <- investigator %>% pull(phone)

# Puzzle 2

name_to_initials <- function(name) {
    initials <- str_extract_all(name, "[A-Z]") %>% unlist %>% paste0(collapse = "")
    return(initials)
}

orders_plus_jd <- orders_plus %>% mutate(name_initials = map_chr(name, name_to_initials)) %>% filter(name_initials == "JD")
orders_coffeebagels_2017 <- orders_plus %>% filter(sku %in% c("DLI1464", "BKY4234", "BKY5887")) %>% group_by(orderid) %>% filter(n() > 1) %>% ungroup %>% filter(year(shipped) == 2017)
contractor_phone <- orders_coffeebagels_2017 %>% select(phone) %>% distinct

                                        # Puzzle 3

spiderguy <- customers %>% filter(year(birthdate) %in% c(2006, 1994, 1982, 1970, 1958, 1946, 1934)) %>% filter( month(birthdate) %in% c(3,4)) %>% filter(citystatezip == "South Ozone Park, NY 11420")
spiderguy_phone <- spiderguy %>% pull(phone)

                                        # Puzzle 4

orders_pastries_before5am <- orders_plus %>% filter(hour(ordered) == 4) %>% filter(ordered == shipped) %>% filter(str_detect(sku, "BKY"))
tinder_girl <- customers %>% filter(customerid == 5375)
tinder_girl_phone <- tinder_girl %>% pull(phone)

                                        # Puzzle 5

orders_queens_cats <- orders_plus %>% filter(str_detect(citystatezip, "Queens")) %>% filter(str_detect(desc, "Cat"))
cat_lady <- orders_queens_cats %>% select(customerid, name, phone) %>% distinct # only one female name

                                        # Puzzle 6

orders_losing <- orders_plus %>% filter(wholesale_cost > unit_price)
frugal_cousin <- customers %>% filter(customerid == 8342) # by far the most lossy orders
frugal_cousin_phone <- frugal_cousin %>% pull(phone)

                                        # Puzzle 7

orders_plus_orderdate <- orders_plus %>% mutate(order_day = day(ordered), order_month = month(ordered), order_year = year(ordered)) %>% mutate(desc_without_colour = (str_replace(desc, "\\([a-z]+\\)", "") %>% str_squish))
frugal_cousin_orders <- orders_plus_orderdate %>% filter(customerid == 8342)
common_orders <- frugal_cousin_orders %>% inner_join(orders_plus_orderdate, by=c("desc_without_colour", "order_day", "order_month", "order_year")) %>% filter(customerid.y != 8342) %>% filter(desc.x != desc.y)
true_love <- common_orders %>% arrange(abs(ordered.x - ordered.y)) %>% select(ordered.x, ordered.y, name.y, phone.y) %>% slice(1)

                                        # Puzzle 8

collector <- orders_plus  %>% filter(str_detect(desc, "^Noah")) %>% group_by(customerid) %>% filter(n() > 200) %>% select(name, phone) %>% distinct
collector_phone <- collector %>% pull(phone)
