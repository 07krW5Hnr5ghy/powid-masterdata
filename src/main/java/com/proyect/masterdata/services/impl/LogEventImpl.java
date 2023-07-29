package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.LogEventMapper;
import com.proyect.masterdata.repository.LogEventRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class LogEventImpl implements IMasterList {
    private final LogEventRepository logEventRepository;
    private final LogEventMapper logEventMapper;
    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return logEventMapper.logEventListToLogEventListDTO(logEventRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            logEventRepository.save(LogEvent.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            LogEvent record = logEventRepository.findById(id).get();
            logEventRepository.save(LogEvent.builder()
                    .name(record.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(record.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            LogEvent logEvent = logEventRepository.save(LogEvent.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return logEventMapper.INSTANCE.logEventToLogEventDTO(logEvent);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
