library(tidyverse)

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))

ggplot(data=diamonds)+
  stat_count(mapping=aes(x=cut))

ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut, y=stat(prop), group=1))

ggplot(data=diamonds)+
  stat_summary(
    mapping=aes(x=cut, y=depth),
    fun.min=min,
    fun.max=max,
    fun=median
  )

ggplot(data=diamonds)+
  geom_pointrange(
    mapping=aes(x=cut, y=depth),
    stat='summary',
    fun.min=min,
    fun.max=max,
    fun=median
  )

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group=1))

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, colour=cut))

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=cut=='Ideal'))

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=clarity))

ggplot(diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5, position='identity')

ggplot(diamonds, mapping=aes(x=cut, color=clarity)) +
  geom_bar(fill=NA, position='identity')

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=clarity), position='fill')

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=clarity), position='dodge')

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy), position='jitter')

ggplot(mpg) +
  geom_jitter(aes(x=displ, y=hwy))

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color=class)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

bar <- ggplot(diamonds) +
  geom_bar(
    aes(x=cut, fill=cut),
    show.legend = FALSE,
    width=1
  ) +
  theme(aspect.ratio=1) +
  labs(x=NULL, y=NULL)
bar + coord_flip()
bar + coord_polar()

ggplot(diamonds) +
  geom_bar(aes(x=cut, fill=clarity)) +
  coord_polar()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Highway MPG",
       x = "Class",
       title = "Highway MPG by car class",
       subtitle = "1999-2008",
       caption = "Source: http://fueleconomy.gov")


ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

this_is_a_really_long_name <- 2.5

seq(1, 10)

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)

filter(diamonds, carat > 3)
