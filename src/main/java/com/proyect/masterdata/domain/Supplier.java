package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSupplier, schema = Constants.schemaLogistics)
public class Supplier {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "supplier_id")
    private UUID id;

    @Column(name = "business_name")
    private String businessName;

    @Column(name = "ruc")
    private String ruc;

    @Column(name = "location")
    private String location;

    @Column(name = "phone_number")
    private String phoneNumber;

    @Column(name = "email")
    private String email;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "supplier_type_id")
    private UUID supplierTypeId;

    @Column(name = "district_id")
    private UUID districtId;

    @Column(name = "country_id")
    private UUID countryId;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "supplier_type_id",columnDefinition = "supplierTypeId",insertable = false,updatable = false)
    private SupplierType supplierType;

    @ManyToOne()
    @JoinColumn(name = "district_id",columnDefinition = "districtId",insertable = false,updatable = false)
    private District district;

    @ManyToOne()
    @JoinColumn(name = "country_id",columnDefinition = "countryId",insertable = false,updatable = false)
    private Country country;
}
