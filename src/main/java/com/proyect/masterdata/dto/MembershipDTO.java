package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class MembershipDTO {
    private UUID id;
    private String user;
    private String membershipState;
    private String subscription;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private OffsetDateTime expirationDate;
}
