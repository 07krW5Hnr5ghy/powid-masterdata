package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface IMembershipPayment {
        ResponseSuccess save(Long membershipId, RequestMembershipPayment requestMembershipPayment, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        PaymentUpdateDTO update(Long membershipId, String invoiceUrl, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        Page<MembershipPayment> list(Double totalPayment, String month, String channel, String sort, String sortColumn,
                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
}
