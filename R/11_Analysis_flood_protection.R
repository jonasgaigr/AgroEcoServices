# A) Infiltration
m_infil <- lmer(log1p(infiltration_adjusted) ~ sample_place + (1 | site_id),
                       data = data_raw)

m_infil_null <- lmer(log1p(infiltration_adjusted) ~ 1 + (1 | site_id),
                     data = data_raw)
anova(m_infil, m_infil_null)
AIC(m_infil, m_infil_null)

# B) Water field capacity
m_wfc <- lmer(WFC_adjusted ~ sample_place * depth_cm +
                infiltration_adjusted + AWS + (1 | site_id),
              data = data_raw)
m_wfc_null <- lmer(WFC_adjusted ~ 1 + (1 | site_id),
                   data = data_raw)
anova(m_wfc, m_wfc_null)
AIC(m_wfc, m_wfc_null)

summary(m_wfc)

# C) AWS
m_aws <- glmer.nb(AWS ~ sample_place * depth_cm +
                    SOC + WFC_adjusted + (1 | site_id),
                  data = data_raw)
m_aws_null <- glmer.nb(AWS ~ 1 + (1 | site_id),
                       data = data_raw)
anova(m_aws, m_aws) 
AIC(m_aws, m_aws_null)


