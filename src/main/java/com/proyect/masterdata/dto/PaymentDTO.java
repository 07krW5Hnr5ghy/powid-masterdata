package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.sql.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PaymentDTO {
    String dni;
    String email;
    String name;
    String surname;
    String phoneNumber;
    String paymentStatusName;
    String billUrl;
    String ecommerceName;
    String paymentMethodName;
    Double totalPaymentPerMonth;
    Number monthsPayed;
    Date startDate;
    Date paymentDate;
    String user;
    Boolean status;
}
