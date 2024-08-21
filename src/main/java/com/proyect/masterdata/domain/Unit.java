package com.proyect.masterdata.domain;

import java.util.Date;

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
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "unit_id")
    private Long id;

    @Column(name = "name")
    private String name;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "unit_type_id")
    private Long unitTypeId;

    @ManyToOne
    @JoinColumn(name = "unit_type_id",columnDefinition = "unitTypeId",insertable = false,updatable = false)
    private UnitType unitType;
}
