package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.repository.SizeRepositoryCustom;
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
import java.util.List;

@Repository
public class SizeRepositoryCustomImpl implements SizeRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Size> searchForSize(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Size> criteriaQuery = criteriaBuilder.createQuery(Size.class);
        Root<Size> itemRoot = criteriaQuery.from(Size.class);
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user, status, criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> sizeList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                sizeList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                sizeList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(sizeList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Size> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name, user, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicateConditions(String name, String user,
            Boolean status, CriteriaBuilder criteriaBuilder, Root<Size> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();
        if (name != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("tokenUser")), user.toUpperCase())));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }
        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Size> itemRoot) {

        List<Order> sizesList = new ArrayList<>();

        if (sortColumn.equals("NAME")) {
            sizesList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equals("tokenUser")) {
            sizesList.add(criteriaBuilder.asc(itemRoot.get("tokenUser")));
        }

        return sizesList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Size> itemRoot) {
        List<Order> sizeList = new ArrayList<>();

        if (sortColumn.equals("NAME")) {
            sizeList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equals("USER")) {
            sizeList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }

        return sizeList;
    }

    private long getOrderCount(String name, String user, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Size> itemRoot = criteriaQuery.from(Size.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user, status, criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
