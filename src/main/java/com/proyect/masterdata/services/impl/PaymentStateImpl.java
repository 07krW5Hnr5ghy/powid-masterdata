package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.request.RequestPaymentStateSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentStateImpl implements IPaymentState {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            paymentStateRepository.save(paymentStateMapper.paymentStateToName(RequestPaymentStateSave.builder()
                    .name(name.toUpperCase()).user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            List<RequestPaymentStateSave> requestPaymentStateSaves = names.stream().map(data -> RequestPaymentStateSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            paymentStateRepository.saveAll(paymentStateMapper.listPaymentStateToListName(requestPaymentStateSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public PaymentStateDTO update(RequestPaymentState requestPaymentState) throws BadRequestExceptions {
        User datauser = userRepository.findById(requestPaymentState.getUser().toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            requestPaymentState.setName(requestPaymentState.getName().toUpperCase());
            requestPaymentState.setUser(requestPaymentState.getUser().toUpperCase());
            PaymentState paymentState = paymentStateMapper.requestPaymentStateToPaymentState(requestPaymentState);
            paymentState.setDateRegistration(new Date(System.currentTimeMillis()));
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.save(paymentState));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            paymentStateRepository.deleteByIdAndUser(code,user.toUpperCase());
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    @Transactional
    public ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                paymentStateRepository.deleteByIdAndUser(data,user.toUpperCase());
            });
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<PaymentStateDTO> list() throws BadRequestExceptions{
        try {
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStateRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<PaymentStateDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStateRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentStateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentStateDTO findByName(String name) throws BadRequestExceptions{
        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<PaymentStateDTO> findByUser(String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStateRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
