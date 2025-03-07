package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderDTO {
    private UUID id;
    private Long orderNumber;
    private String sellerName;
    private String customerName;
    private String customerType;
    private String customerPhone;
    private String customerAddress;
    private String instagram;
    private String department;
    private String province;
    private String district;
    private String address;
    private String managementType;
    private String paymentMethod;
    private String paymentState;
    private String saleChannel;
    private String reference;
    private String deliveryPoint;
    private String discount;
    private String dni;
    private List<String> paymentReceipts;
    private Boolean receiptFlag;
    private List<String> courierPictures;
    private Boolean deliveryFlag;
    private BigDecimal saleAmount;
    private BigDecimal deliveryAmount;
    private BigDecimal advancedPayment;
    private BigDecimal duePayment;
    private BigDecimal discountAmount;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String deliveryAddress;
    private String courier;
    private String orderStatus;
    private String observations;
    private String closingChannel;
    private List<OrderItemDTO> orderItemDTOS;
    private String store;
    private String orderStateColor;
    private String cancellationReason;
    private List<OrderLogDTO> orderLogs;
    private String user;
}
