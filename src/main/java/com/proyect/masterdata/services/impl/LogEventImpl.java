package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.LogEventMapper;
import com.proyect.masterdata.repository.LogEventRepository;
import com.proyect.masterdata.services.ILogEvent;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LogEventImpl implements ILogEvent {
    private final LogEventRepository logEventRepository;
    private final LogEventMapper logEventMapper;
    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            logEventRepository.save(logEventMapper.logEventToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            logEventRepository.saveAll(logEventMapper.listLogEventToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public LogEventDTO update(RequestLogEvent requestLogEvent) throws BadRequestExceptions {
        try {
            requestLogEvent.setName(requestLogEvent.getName().toUpperCase());
            LogEvent updatedLogEvent = logEventMapper.requestLogEventToLogEvent(requestLogEvent);
            updatedLogEvent.setDateRegistration(new Date(System.currentTimeMillis()));
            LogEvent logEvent = logEventRepository.save(updatedLogEvent);
            return logEventMapper.logEventToLogEventDTO(logEvent);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            logEventRepository.deleteById(code);
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
            logEventRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<LogEventDTO> list() throws BadRequestExceptions{
        try {
            return logEventMapper.listLogEventToListLogEventDTO(logEventRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public LogEventDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return logEventMapper.logEventToLogEventDTO(logEventRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public LogEventDTO findByName(String name) throws BadRequestExceptions{
        try {
            return logEventMapper.logEventToLogEventDTO(logEventRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
