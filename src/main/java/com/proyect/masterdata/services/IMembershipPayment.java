package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestMembershipPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IMembershipPayment {
        ResponseSuccess save(RequestMembershipPayment requestMembershipPayment, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        PaymentUpdateDTO update(Long membershipId, RequestMembershipPaymentUpdate requestMembershipPaymentUpdate,
                        String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        Page<MembershipPayment> list(Double totalPayment, String month, String channel, String sort, String sortColumn,
                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
}
