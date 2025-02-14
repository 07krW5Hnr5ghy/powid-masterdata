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

import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOnboardingModule, schema = Constants.schemaManagement)
public class OnboardModule {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "onboard_module_id")
    private UUID id;

    @Column(name = "onboard_id")
    private UUID onboardId;

    @Column(name = "module_id")
    private UUID moduleId;

    @ManyToOne
    @JoinColumn(name = "onboard_id", columnDefinition = "onboardId", insertable = false, updatable = false)
    private Onboard onboard;

    @ManyToOne
    @JoinColumn(name = "module_id", columnDefinition = "moduleId", insertable = false, updatable = false)
    private Module module;
}
