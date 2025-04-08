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
@Table(name = Constants.tablePaymentMetodClient,schema = Constants.schemaManagement)
public class PaymentMetodClient {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "payment_metod_client_id")
    private UUID id;

    @Column(name = "account_detail")
    private String accountDetail;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "payment_method_id")
    private UUID paymentMethodId;

    @Column(name = "status")
    private boolean status;

    @Column(name = "observations",columnDefinition = "text")
    private String observations;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @ManyToOne()
    @JoinColumn(name = "payment_method_id", columnDefinition = "paymentMethodId",insertable = false,updatable = false)
    private OrderPaymentMethod orderPaymentMethod;

    @ManyToOne()
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
