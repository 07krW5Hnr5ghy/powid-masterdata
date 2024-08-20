package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.repository.ColorRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Repository
public class ColorRepositoryCustomImpl implements ColorRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Color> searchForColor(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Color> criteriaQuery = criteriaBuilder.createQuery(Color.class);
        Root<Color> itemRoot = criteriaQuery.from(Color.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> colorList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                colorList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                colorList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(colorList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Color> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Color> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Color> itemRoot) {
        List<Order> colorList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("NAME")) {
            colorList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            colorList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            colorList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            colorList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            colorList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return colorList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Color> itemRoot) {
        List<Order> colorList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("NAME")) {
            colorList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            colorList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            colorList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            colorList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            colorList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return colorList;
    }

    private long getOrderCount(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Color> itemRoot = criteriaQuery.from(Color.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
