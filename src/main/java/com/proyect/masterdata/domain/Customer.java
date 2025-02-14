package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCustomer,schema = Constants.schemaOrder)
public class Customer {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "customer_id")
    private String id;

    @Column(name = "name")
    private String name;

    @Column(name = "instagram")
    private String instagram;

    @Column(name = "phone")
    private String phone;

    @Column(name = "address")
    private String address;

    @Column(name = "reference")
    private String reference;

    @Column(name = "dni")
    private String dni;

    @Column(name = "customer_type_id")
    private Long customerTypeId;

    @Column(name = "district_id")
    private Long districtId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @ManyToOne
    @JoinColumn(name = "customer_type_id",columnDefinition = "customerTypeId",insertable = false,updatable = false)
    private CustomerType customerType;

    @ManyToOne
    @JoinColumn(name = "district_id", columnDefinition = "districtId", insertable = false, updatable = false)
    private District district;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false,updatable = false)
    private Client client;
}
