package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCourier,schema = Constants.schemaLogistics)
public class Courier {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "courier_id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "username")
    private String username;

    @Column(name = "phone")
    private String phone;

    @Column(name = "dni")
    private String dni;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "address")
    private String address;

    @Column(name = "plate")
    private String plate;

    @Column(name = "registration_date")
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    private OffsetDateTime updateDate;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name="delivery_company_id")
    private UUID deliveryCompanyId;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="delivery_company_id",columnDefinition = "deliveryCompanyId",insertable = false,updatable = false)
    private DeliveryCompany deliveryCompany;

}
