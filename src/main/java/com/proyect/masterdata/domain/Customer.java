package com.proyect.masterdata.domain;

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
@Table(name = Constants.tableCustomer,schema = Constants.schemaOrder)
public class Customer {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "customer_id")
    private Long id;

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

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "district_id")
    private Long districtId;

    @Column(name = "province_id")
    private Long provinceId;

    @Column(name = "department_id")
    private Long departmentId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "customer_type_id")
    private Long customerTypeId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "department_id",columnDefinition = "departmentId",insertable = false,updatable = false)
    private Department department;

    @ManyToOne
    @JoinColumn(name = "district_id", columnDefinition = "districtId", insertable = false, updatable = false)
    private District district;

    @ManyToOne
    @JoinColumn(name = "province_id", columnDefinition = "provinceId", insertable = false, updatable = false)
    private Province province;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "customer_type_id",columnDefinition = "customerTypeId",insertable = false,updatable = false)
    private CustomerType customerType;

}
