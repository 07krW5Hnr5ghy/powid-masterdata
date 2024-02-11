package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
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
@Table(name = Constants.tableSale, schema = Constants.schemaOrder)
public class Sale {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "sale_id")
    private Long id;

    @Column(name = "seller")
    private String seller;

    @Column(name = "observations")
    private String observations;

    @Column(name = "delivery_address")
    private String deliveryAddress;

    @Column(name = "sale_amount")
    private Double saleAmount;

    @Column(name = "delivery_amount")
    private Double deliveryAmount;

    @Column(name = "advance_payment")
    private Double advancePayment;

    @Column(name = "due_payment")
    private Double duePayment;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "sale_channel_id")
    private Long saleChannelId;

    @Column(name = "payment_state_id")
    private Long paymentStateId;

    @Column(name = "payment_method_id")
    private Long paymentMethodId;

    @Column(name = "management_type_id")
    private Long managementTypeId;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "sale_channel_id", columnDefinition = "saleChannelId", insertable = false, updatable = false)
    private SaleChannel saleChannel;

    @ManyToOne
    @JoinColumn(name = "payment_state_id", columnDefinition = "paymentStateId", insertable = false, updatable = false)
    private PaymentState paymentState;

    @ManyToOne
    @JoinColumn(name = "payment_method_id", columnDefinition = "paymentMethodId", insertable = false, updatable = false)
    private PaymentMethod paymentMethod;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "management_type_id", columnDefinition = "managementTypeId", insertable = false, updatable = false)
    private ManagementType managementType;

}
