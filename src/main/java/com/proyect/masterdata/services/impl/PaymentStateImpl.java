package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestCreatePaymentState;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.services.IPaymentState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PaymentStateImpl implements IPaymentState {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        try {
            paymentStateRepository.save(paymentStateMapper.paymentStateToName(name.toUpperCase(),user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestCreatePaymentState> requestCreatePaymentStateList) throws BadRequestExceptions{
        try {
            paymentStateRepository.saveAll(paymentStateMapper.listRequestCreatePaymentStateToListPaymentState(requestCreatePaymentStateList)
                    .stream()
                    .map(
                            c -> {
                                PaymentState paymentState = new PaymentState();
                                paymentState.setName(c.getName().toUpperCase());
                                paymentState.setStatus(c.getStatus());
                                paymentState.setUser(c.getUser().toUpperCase());
                                return paymentState;
                            }
                    ).collect(Collectors.toList())
            );
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
        try {
            requestPaymentState.setName(requestPaymentState.getName().toUpperCase());
            requestPaymentState.setUser(requestPaymentState.getUser().toUpperCase());
            PaymentState updatedPaymentState = paymentStateMapper.requestPaymentStateToPaymentState(requestPaymentState);
            updatedPaymentState.setDateRegistration(new Date(System.currentTimeMillis()));
            PaymentState paymentState = paymentStateRepository.save(updatedPaymentState);
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentState);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            paymentStateRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            paymentStateRepository.deleteAllById(codes);
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
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStateRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentStateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentStateDTO findByName(String name) throws BadRequestExceptions{
        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
