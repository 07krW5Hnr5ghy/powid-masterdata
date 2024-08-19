package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

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

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrder, schema = Constants.schemaOrder)
public class Ordering {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "order_id")
    private Long id;

    @Column(name = "cancellation")
    private Boolean cancellation;

    @Column(name = "seller")
    private String seller;

    @Column(name = "observations")
    private String observations;

    @Column(name = "delivery_address")
    private String deliveryAddress;

    @Column(name = "delivery_amount")
    private Double deliveryAmount;

    @Column(name = "discount_amount")
    private Double discountAmount;

    @Column(name = "advanced_payment")
    private Double advancedPayment;

    @Column(name = "receiptFlag")
    private Boolean receiptFlag;

    @Column(name = "deliveryFlag")
    private Boolean deliveryFlag;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "order_state_id")
    private Long orderStateId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "courier_id")
    private Long courierId;

    @Column(name = "delivery_point_id")
    private Long deliveryPointId;

    @Column(name = "payment_state_id")
    private Long paymentStateId;

    @Column(name = "payment_method_id")
    private Long paymentMethodId;

    @Column(name = "sale_channel_id")
    private Long saleChannelId;

    @Column(name = "management_type_id")
    private Long managementTypeId;

    @Column(name = "store_id")
    private Long storeId;

    @Column(name = "closing_channel_id")
    private Long closingChannelId;

    @Column(name = "customer_id")
    private Long customerId;

    @Column(name = "discount_id")
    private Long discountId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "order_state_id", columnDefinition = "orderStateId", insertable = false, updatable = false)
    private OrderState orderState;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false,updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "courier_id",columnDefinition = "courierId",insertable = false,updatable = false)
    private Courier courier;

    @ManyToOne()
    @JoinColumn(name = "payment_state_id", columnDefinition = "paymentStateId",insertable = false,updatable = false)
    private OrderPaymentState orderPaymentState;

    @ManyToOne()
    @JoinColumn(name = "payment_method_id", columnDefinition = "paymentMethodId",insertable = false,updatable = false)
    private OrderPaymentMethod orderPaymentMethod;

    @ManyToOne()
    @JoinColumn(name = "sale_channel_id",columnDefinition = "saleChannelId",insertable = false,updatable = false)
    private SaleChannel saleChannel;

    @ManyToOne
    @JoinColumn(name = "management_type_id", columnDefinition = "managementTypeId", insertable = false, updatable = false)
    private ManagementType managementType;

    @ManyToOne
    @JoinColumn(name = "store_id", columnDefinition = "storeId", insertable = false, updatable = false)
    private Store store;

    @ManyToOne
    @JoinColumn(name = "closing_channel_id",columnDefinition = "closingChannelId",insertable = false,updatable = false)
    private ClosingChannel closingChannel;

    @ManyToOne
    @JoinColumn(name = "customer_id",columnDefinition = "customerId",insertable = false,updatable = false)
    private Customer customer;

    @ManyToOne
    @JoinColumn(name = "delivery_point_id",columnDefinition = "deliveryPointId",insertable = false,updatable = false)
    private DeliveryPoint deliveryPoint;

    @ManyToOne
    @JoinColumn(name = "discount_id",columnDefinition = "discountId",insertable = false,updatable = false)
    private Discount discount;

}
