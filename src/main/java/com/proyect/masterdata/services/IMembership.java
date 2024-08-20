package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IMembership {
    CompletableFuture<Membership> save(User user, MembershipPayment membershipPayment, String subscriptionName, List<String> modules, Boolean demo, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<MembershipDTO>> list(
            String user,
            String membershipState,
            String subscription,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<MembershipDTO>> listFalse(
            String user,
            String membershipState,
            String subscription,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
