package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PaymentDTO {
    private String dni;
    private String email;
    private String name;
    private String surname;
    private String phoneNumber;
    private double totalPayment;
    private double discount;
    private String month;
    private String channel;
    private String ecommerce;
    private String user;
    private String paymentMethod;
    private Date starDate;
    private Date paymentDate;
}
