package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOnboardingChannel, schema = Constants.schemaManagement)
public class OnboardingChannel {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_onboarding_canal", unique = true)
    private Long id;

    @Column(name = "id_onboarding", nullable = false)
    private Long onboardingId;

    @Column(name = "id_closing_channel", nullable = false)
    private Long closingChannelId;

    @OneToOne
    @JoinColumn(name = "id_onboarding", columnDefinition = "onboardingId", insertable = false)
    private Onboarding onboarding;
}
