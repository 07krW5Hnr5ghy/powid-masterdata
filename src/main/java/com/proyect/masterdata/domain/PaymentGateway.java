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
@Table(name = Constants.tablePaymentGateway,schema = Constants.schemaPayment)
public class PaymentGateway {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "payment_gateway_id", nullable = false)
    private String id;

    @Column(name = "name")
    private String name;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "status")
    private Boolean status;

}
