package com.proyect.masterdata.domain;

import java.time.OffsetDateTime;
import java.util.UUID;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableUnit, schema = Constants.schemaMaster)
public class Unit {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "unit_id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    private OffsetDateTime updateDate;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "unit_type_id")
    private UUID unitTypeId;

    @ManyToOne
    @JoinColumn(name = "unit_type_id",columnDefinition = "unitTypeId",insertable = false,updatable = false)
    private UnitType unitType;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
