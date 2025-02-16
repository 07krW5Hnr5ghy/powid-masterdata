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

import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOnboardingStore, schema = Constants.schemaManagement)
public class OnboardStore {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "onboard_store_id")
    private UUID id;

    @Column(name = "onboard_id", nullable = false)
    private UUID onboardId;

    @Column(name = "store_id", nullable = false)
    private UUID storeId;

    @OneToOne
    @JoinColumn(name = "store_id", columnDefinition = "storeId", insertable = false, updatable = false)
    private Store store;

    @OneToOne
    @JoinColumn(name = "onboard_id", columnDefinition = "onboardId", insertable = false, updatable = false)
    private Onboard onboard;

}
