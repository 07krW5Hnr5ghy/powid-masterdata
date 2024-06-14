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
public class MembershipDTO {
    private String membershipState;
    private String subscription;
    private Date registrationDate;
    private Date updateDate;
    private Date expirationDate;
}
