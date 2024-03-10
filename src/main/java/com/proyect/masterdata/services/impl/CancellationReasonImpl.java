package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CancellationReason;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CancellationReasonRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICancellationReason;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class CancellationReasonImpl implements ICancellationReason {

    private final CancellationReasonRepository cancellationReasonRepository;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        CancellationReason cancellationReason;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            cancellationReason = cancellationReasonRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(cancellationReason != null){
            throw new BadRequestExceptions(Constants.ErrorCancellationReasonExists);
        }

        try{

            cancellationReasonRepository.save(CancellationReason.builder()
                            .name(name.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<String> list() throws BadRequestExceptions {
        List<CancellationReason> cancellationReasonList = new ArrayList<>();

        try{
            cancellationReasonList = cancellationReasonRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(cancellationReasonList.isEmpty()){
            return Collections.emptyList();
        }

        return cancellationReasonList.stream().map(CancellationReason::getName).toList();
    }

    @Override
    public List<String> listFalse() throws BadRequestExceptions {
        List<CancellationReason> cancellationReasonList = new ArrayList<>();

        try{
            cancellationReasonList = cancellationReasonRepository.findAllByStatusFalse();
        }catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(cancellationReasonList.isEmpty()){
            return Collections.emptyList();
        }

        return cancellationReasonList.stream().map(CancellationReason::getName).toList();
    }

    @Override
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        CancellationReason cancellationReason;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            cancellationReason = cancellationReasonRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(cancellationReason == null){
            throw new BadRequestExceptions(Constants.ErrorCancellationReason);
        }

        try{
            cancellationReason.setStatus(false);
            cancellationReason.setUpdateDate(new Date(System.currentTimeMillis()));
            cancellationReasonRepository.save(cancellationReason);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        CancellationReason cancellationReason;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            cancellationReason = cancellationReasonRepository.findByNameAndStatusFalse(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(cancellationReason == null){
            throw new BadRequestExceptions(Constants.ErrorCancellationReason);
        }

        try{
            cancellationReason.setStatus(true);
            cancellationReason.setUpdateDate(new Date(System.currentTimeMillis()));
            cancellationReasonRepository.save(cancellationReason);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
