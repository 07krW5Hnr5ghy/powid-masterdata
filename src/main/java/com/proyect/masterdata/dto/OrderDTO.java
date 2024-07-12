package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderDTO {
    private Long id;
    private String sellerName;
    private String customerName;
    private String customerType;
    private String customerPhone;
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
    private List<String> paymentReceipts;
    private List<String> courierPictures;
    private BigDecimal saleAmount;
    private BigDecimal deliveryAmount;
    private BigDecimal advancedPayment;
    private BigDecimal duePayment;
    private Date registrationDate;
    private Date updateDate;
    private String deliveryAddress;
    private String courier;
    private String orderStatus;
    private String observations;
    private String closingChannel;
}
