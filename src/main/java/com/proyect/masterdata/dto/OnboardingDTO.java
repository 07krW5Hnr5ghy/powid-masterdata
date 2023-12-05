package com.proyect.masterdata.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OnboardingDTO {

    private String username;

    private String name;

    private String surname;

    private String email;

    private String address;

    private String mobile;

    private String dni;

    private String category;

    private Long usersMinimum;

    private Long usersMaximum;

    private Boolean ecommerce;

    private Boolean billing;

    private String comment;

    private String businessName;

    private String businessRuc;

    private String gender;

    private String password;

    private String district;

    private String store;

    private String storeUrl;

    private String storeType;

    private List<String> closingChannels;

    private String entryChannel;

    private Boolean demo;
}
