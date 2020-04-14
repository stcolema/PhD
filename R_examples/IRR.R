
library(polynom)
library(tidyverse)
library(patchwork)
library(ggthemes)


IRR <- 10

value <- 1e6
end <- 40

findContribution <- function(value, irr, n_years){
  n_irr <- length(irr)
  my_data <- list()
  base_df <- data.frame(Time=1:end, Value=value)
  for(i in 1:n_irr){
    my_data[[i]] <- base_df %>% 
      mutate(IRR=irr[i], 
             Contribution=Value/((1+IRR)**(Time))
             )
  }
  
  out_df <- do.call(bind_rows, my_data)
  
  out_df$IRR <- factor(out_df$IRR)
  
  out_df
}

contributionManyValues <- function(values, irr, n_years){
  
  n_values <- length(values)
  
  my_data <- list()
  for(i in 1:n_values){
    my_data[[i]] <- values[i] %>% 
      findContribution(irr, n_years = n_years) %>% 
      mutate(Value = values[i])
    
  }
  
  out_df <- do.call(bind_rows, my_data)
  
  out_df$Value <- factor(out_df$Value)
  
  out_df
  
}

my_data <- findContribution(1e6, 0.01*c(0, 1, 2, 5, 10, 20), 25)

my_data %>% ggplot(aes(x=Time, y=Contribution,group = IRR))+
  geom_line(aes(colour=IRR))+
  labs(
    title = paste0("Value of ", value, " over ", 25, " periods for various discount rates")
  )+
  scale_colour_viridis_d() +
  # geom_hline(yintercept = value * c(0.5, 0.25, 0.1), lty =3, colour = "orange") +
  theme_clean()

my_data %>% 
  filter(Value == 5e5)

data <- data.frame(Time=1:end, Value=value) %>% 
  mutate(Contribution=Value/((1+IRR)**(Time)))


ggplot(data,aes(x=Time, y=Contribution))+
  geom_line()+
  labs(
    title = paste0("Value of ", value, " at different period for IRR of ", IRR, "%")
  )

new_flows <- data.frame(Time=1:end,
                        Values=c(rep(5e6,3), rep(0,10), rep(-1e6, 20), rep(0, 7)))

ggplot(new_flows, aes(x=Time,y=Values))+
  geom_histogram(stat="identity")

total_value <- 100e6

log(1:1000) %>% plot()

ramp_up_start <- 1e5
ramp_up_length <- 5
inflation <- 0.03
ramp_up <- 1e5 * log(2:(ramp_up_length+2))
investment <- 1.7685e6
construciton_period <- 5

plot(ramp_up)
cashflows <- data.frame(Time=1:end, Inflation=(1+inflation)**(1:end)) %>% 
  mutate(Cashflow_real=c(ramp_up[1:ramp_up_length], rep(ramp_up[ramp_up_length+1], end- ramp_up_length)),
         Cashflow_inf=Cashflow_real*Inflation
         )

cashflows <- data.frame(Time=1:end, Inflation=(1+inflation)**(1:end)) %>% 
  mutate(Cashflow_real=c(
    -investment, 
    rep(0, construciton_period-1),
    ramp_up[1:ramp_up_length], 
    rep(ramp_up[ramp_up_length+1], end- ramp_up_length- construciton_period )),
         Cashflow_inf=Cashflow_real*Inflation
  )



cashflows %>% 
  ggplot(aes(x=Time))+
  geom_point(aes(y=Cashflow_inf))+
  geom_point(aes(y=Cashflow_real))


p <- polynomial(cashflows$Cashflow_inf)
x <- solve(p)

posisble_sol <- 1/x - 1

irr <- posisble_sol[which(posisble_sol == Mod(posisble_sol))] %>% Re()
irr

my_data <- cashflows %>% 
  pivot_longer(contains("Cashflow"),names_to = "Type", values_to = "Cashflow") %>% 
  dplyr::mutate(Inflated=(Type=="Cashflow_inf"),
                IRR_weight=1/(1+irr)**Time,
                Contribution=Cashflow*IRR_weight)

p1 <- my_data %>% 
  ggplot(aes(x=Time, y=Cashflow,colour=Inflated))+
  geom_line() 

p2 <- my_data %>% 
  ggplot(aes(x=Time, y=Contribution,colour=Inflated))+
  geom_line()

p1 + p2

p3 <- my_data %>% filter(Inflated) %>% 
  ggplot(aes(x=Time))+
  geom_line(aes(y=Contribution,colour = "red"))+
  geom_line(aes(y=Cashflow,colour="blue"))+
  labs(
    title="Comparison of cashflow to value contribution",
    subtitle=paste("IRR of", round(irr*100,digits=2), "%")
  )+
  scale_color_identity(name = "Quantity",
                       breaks = c("red", "blue"),
                       labels = c("Cashflow", "Contribution"),
                       guide = "legend")+
  theme_bw()
p3

low_irr <- 0.02

low_irr_data <- cashflows %>% 
  pivot_longer(contains("Cashflow"),names_to = "Type", values_to = "Cashflow") %>% 
  dplyr::mutate(Inflated=(Type=="Cashflow_inf"),
                IRR_weight=1/(1+low_irr)**Time,
                Contribution=Cashflow*IRR_weight)

p4 <- low_irr_data %>% filter(Inflated) %>% 
  ggplot(aes(x=Time))+
  geom_line(aes(y=Contribution,colour = "red"))+
  geom_line(aes(y=Cashflow,colour="blue"))+
  labs(
    title="Comparison of cashflow to value contribution",
    subtitle=paste("IRR of", round(low_irr*100,digits=2), "%")
  )+
  scale_color_identity(name = "Quantity",
                       breaks = c("red", "blue"),
                       labels = c("Cashflow", "Contribution"),
                       guide = "legend")+
  theme_bw()
p4

comp_data <- bind_rows(my_data %>% mutate(IRR=round(irr, digits = 4)),
                       low_irr_data %>% mutate(IRR=round(low_irr,digits=4))
)

comp_data$IRR <- factor(comp_data$IRR)

comp_data %>% filter(Inflated) %>% 
  ggplot(aes(x=Time, y=Contribution,group=IRR))+
  geom_line(aes(colour=IRR))

comp_data$Contribution
comp_data$IRR

comp_data[81,]
nrow(comp_data)
