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

import java.util.List;

public interface IMembership {

    Membership save(User user, MembershipPayment membershipPayment, String subscriptionName, List<String> modules, Boolean demo, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String clientRuc, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<MembershipDTO> list(String channel, String module, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
