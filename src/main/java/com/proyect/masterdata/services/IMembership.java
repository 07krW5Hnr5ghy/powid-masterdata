package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface IMembership {

    ResponseSuccess save(String clientRuc, String subscriptionName, Boolean demo, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String clientRuc, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<MembershipDTO> list(String channel, String module, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
