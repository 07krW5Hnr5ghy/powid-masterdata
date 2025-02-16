package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ColorMapper;
import com.proyect.masterdata.repository.ColorRepository;
import com.proyect.masterdata.repository.ColorRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IColor;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ColorImpl implements IColor {
    private final ColorRepository colorRepository;
    private final ColorMapper colorMapper;
    private final UserRepository userRepository;
    private final ColorRepositoryCustom colorRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Color color;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                color = colorRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (color != null) {
                throw new BadRequestExceptions(Constants.ErrorColorExists.toUpperCase());
            }

            try {
                Color newColor = colorRepository.save(Color.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                                .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_COLOR","COLOR "+newColor.getName()+" CREADO.",newColor.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Color color;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                color = colorRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (color == null) {
                throw new BadRequestExceptions(Constants.ErrorColor.toUpperCase());
            }

            try {
                color.setStatus(false);
                color.setUpdateDate(OffsetDateTime.now());
                color.setUser(user);
                color.setUserId(user.getId());
                colorRepository.save(color);
                iAudit.save("DELETE_COLOR","COLOR "+color.getName()+" DESACTIVADO.", color.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Color color;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                color = colorRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (color == null) {
                throw new BadRequestExceptions(Constants.ErrorColor.toUpperCase());
            }

            try {
                color.setStatus(true);
                color.setUpdateDate(OffsetDateTime.now());
                color.setUser(user);
                color.setUserId(user.getId());
                colorRepository.save(color);
                iAudit.save("ACTIVATE_COLOR","COLOR "+color.getName()+" ACTIVADO.",color.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<ColorDTO>> listColor() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Color> colors;
            try {
                colors = colorRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (colors.isEmpty()) {
                return Collections.emptyList();
            }
            return colorMapper.listColorToListColorDTO(colors);
        });
    }

    @Override
    public CompletableFuture<Page<ColorDTO>> list(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Color> colorPage;
            try {
                colorPage = colorRepositoryCustom.searchForColor(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (colorPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(colorMapper.listColorToListColorDTO(colorPage.getContent()),
                    colorPage.getPageable(), colorPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ColorDTO>> listStatusFalse(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Color> colorPage;
            try {
                colorPage = colorRepositoryCustom.searchForColor(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (colorPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            return new PageImpl<>(colorMapper.listColorToListColorDTO(colorPage.getContent()),
                    colorPage.getPageable(), colorPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ColorDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Color> colors;
            try {
                colors = colorRepository.findAll();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (colors.isEmpty()) {
                return Collections.emptyList();
            }
            return colorMapper.listColorToListColorDTO(colors);
        });
    }
}
