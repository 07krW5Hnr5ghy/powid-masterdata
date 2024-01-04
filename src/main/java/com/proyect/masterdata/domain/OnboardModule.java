package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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
@Table(name = Constants.tableOnboardingModule, schema = Constants.schemaManagement)
public class OnboardModule {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_onboarding_modulo")
    private Long id;

    @Column(name = "id_onboarding")
    private Long onboardId;

    @Column(name = "id_modulo")
    private Long moduleId;

    @ManyToOne
    @JoinColumn(name = "id_onboarding", columnDefinition = "onboardId", insertable = false, updatable = false)
    private Onboard onboard;

    @ManyToOne
    @JoinColumn(name = "id_modulo", columnDefinition = "moduleId", insertable = false, updatable = false)
    private Module module;
}
