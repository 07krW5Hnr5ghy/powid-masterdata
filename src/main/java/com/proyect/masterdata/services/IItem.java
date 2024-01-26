package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IItem {
    public ResponseSuccess save(Ordering ordering, RequestItem requestItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
