
install.packages("remotes")
remotes::install_github("gary-au/pt")

library("pt")

choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
objective_consequences <- c(2500, 2400, 0, 2400,
                            2500, 0, 2400, 0)
probability_strings <- c("0.33", "0.66", "0.01", "1.0",
                         "0.33", "0.67", "0.34", "0.66")

my_choices <- Choices(choice_ids=choice_ids,
                      gamble_ids=gamble_ids,
                      outcome_ids=outcome_ids,
                      objective_consequences=objective_consequences,
                      probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
            decision_square_x=0.2, decision_square_edge_length=0.05,
            circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03,
            probability_text_digits=4, y_probability_text_offset=0.015,
            y_value_text_offset=0.005, x_value_text_offset=0.025,
            probability_text_font_colour="red", probability_text_font_size=11,
            objective_consequence_text_font_colour="blue",
            objective_consequence_text_font_size=11, label=c("A","B","C", "D"),
            label_font_colour=c("orange","magenta","green","blue"),
            label_font_size=c(11,11,11,11),
            label_positions=list(c(0.26,0.85),c(0.26,0.55),
                                 c(0.26,0.4),c(0.26,0.1)))

compareEV(my_choices, digits=4)

my_utility <- Utility(fun="power", par=c(alpha=0.63, beta=0.63, lambda=2.25))
compareEU(my_choices, utility=my_utility, digits=4)

my_utility <- Utility(fun="power", par=c(alpha=1.2, beta=1.2, lambda=2.25))
compareEU(my_choices, utility=my_utility, digits=4)

my_utility <- Utility(fun="power", par=c(alpha=1.0, beta=1.0, lambda=1.0))
compareEU(my_choices, utility=my_utility, digits=4)


tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))

linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))

comparePT(my_choices,
          prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
          prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
          utility=tk_1992_utility, digits=4)

my_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
drawSimplex(x1=0, x2=2400, x3=2500,
            line_dot_density=3000,
            draw_ev_flag=TRUE, ev_colour="black",
            draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red",
            draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
            start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),
                              c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)),
            labels=c("A","B","C","D","increasing preference"),
            label_positions=list(c(0.05,0.38),c(0.05,0.05),c(0.7,0.38),
                                 c(0.7,0.05),c(0.7,0.7)),
            label_colours=c("orange","magenta","green","blue","red"),
            label_font_sizes=c(12,12,12,12,16),
            label_font_faces=c("plain","plain","plain","plain","bold"),
            label_rotations=c(0,0,0,0,-45),
            circle_radii=c(0.01,0.01,0.01,0.01),
            circle_outline_colours=c("black","black","black","black"),
            circle_fill_colours=c("orange","purple","orange","purple"),
            circle_positions=list(c(0.01,0.33),c(0,0),
                                  c(0.67,0.33),c(0.66,0)),
            lines=list(c(0.01,0.33,0,0),c(0,0,0.66,0),
                       c(0.66,0,0.67,0.33),c(0.67,0.33,0.01,0.33)),
            line_widths=c(1, 1, 1, 1),
            line_styles=c("dashed", "dashed", "dashed", "dashed"),
            line_colours=c("blue","blue","blue","blue"),
            arrows=list(c(0.8,0.5,0.5,0.8)),
            arrow_widths=c(2), arrow_styles=c("solid"), arrow_colours=c("red"))
