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
public class OnboardingStore {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_onboarding_tienda", unique = true)
    private Long id;

    @Column(name = "id_onboarding", nullable = false, unique = true)
    private Long onboardingId;

    @Column(name = "id_store", nullable = false)
    private Long storeId;

    @OneToOne
    @JoinColumn(name = "id_store", columnDefinition = "storeId", insertable = false)
    private Store store;
}
